{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Control.Err
Copyright   : Public Domain
Description : Emit, Throwing, Collecting, and Catching Errors
Stability   : experimental
-}

module Control.Err
  ( -- * Loquacious Errors with Call Stacks.
    Err
  , toErr
  , fromErr
    -- * Throwing and Catching Errors
  , Amend(..)
  , amendCollected
  , amendCaught
  , Emit(..)
  , Collect(..)
  , Throw(..)
  , Catch(..)
    -- * Throwing and Catching Loquacious Types
  , emit
  , throw
  , catch
  , note
  , (?)
  ) where

import           Control.Exception (Exception(fromException, toException), throwIO)
import qualified Control.Exception as IO
import           Data.Foldable (foldl')
import           Data.Maybe (catMaybes)
import           Data.Text.Prettyprint.Doc (align, nest, sep, softline, vsep, (<+>))
import           Data.Typeable (cast)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack, callStack, getCallStack, withFrozenCallStack)
import           Text.Loquacious (Loq, Loquacious(loq, toLoq), defLoq)

-- | Encapsulate a 'Loq' with a Call Stack and other 'Loq' about it.
data Err = HasCallStack => Err [Loq] Loq

instance Show Err where
  show = show . defLoq

instance Exception Err

instance Loquacious Err where
  loq m (Err ms x) = m x <> nest 2 (softline <> align (vsep $ catMaybes [stack, notes]))
    where
      stack | null (getCallStack callStack) = Nothing
            | otherwise = Just $ "@" <+> m callStack
      notes | null ms = Nothing
            | otherwise = Just $ "?" <+> (align . sep $ m <$> ms)

-- | Embed a value with a Loquacious instance in an 'Err'.
toErr :: (HasCallStack, Loquacious t) => t -> Err
toErr = withFrozenCallStack $ Err [] . toLoq

-- | Attempt to extract a value with a Loquacious instance from an 'Err'.
fromErr :: Loquacious t => Err -> Maybe t
fromErr (Err _ x) = cast x

-- | Amending 'Err's, whether 'Throw'n or 'Collect'ed
class Amend f where
  amendErr :: f a -> (Err -> Err) -> f a
  default amendErr :: Catch f => f a -> (Err -> Err) -> f a
  amendErr = amendCaught

-- | Helper to amend 'Catch'able 'Err's
amendCaught :: Catch f => f a -> (Err -> Err) -> f a
amendCaught a f = catch a (throw . f)

-- | Helper to amend 'Collect'able 'Err's
amendCollected :: Collect f => f a -> (Err -> Err) -> f a
amendCollected a f = foldl' emitErr (clearErr a) (f <$> collectErr a)

-- | Polymorphic types that support emitting 'Err's
--
-- Note that this does not imply that the type do anything with it or keep it
-- around, so pretty much every container type can "emit 'Err's".
class Emit f where
  -- | Emit an 'Err', but continue on the normal computation path.
  --
  -- Emitted 'Err's are more suited for warnings than errors.
  emitErr :: HasCallStack => f a -> Err -> f a

-- | Polymorphic types that support collecting previously 'Emit'ed 'Err's.
--
-- Most types don't support anything like this unless they are designed for it.
class Emit f => Collect f where
  -- | Collect 'Err's that were previously 'Emit'ed.
  collectErr :: f a -> [Err]
  -- | Clear previously 'Emit'ed messages.
  clearErr :: f a -> f a

-- | Types that support error short-circuiting.
--
-- Compare with the @MonadThrow@ class from the @exceptions@ package. In
-- contrast to that type, there is no requirement that the type be a 'Monad',
-- indeed, 'throwErr' is akin to 'pure' from 'Applicative'.
class Throw f where
  -- | Throw an 'Err', invoking an exceptional computation path.
  throwErr :: HasCallStack => Err -> f a

-- | Types that support recovering from thrown errors.
--
-- Compare with the @MonadCatch@ class from the @exceptions@ package. Like the
-- 'Throw' class there's no 'Monad' requirement, though catching errors does
-- mirror '>>='.
class Throw f => Catch f where
  -- | Catch an 'Err', possibly resumably normal computation.
  catchErr :: f a -> (Err -> f a) -> f a

-- | Message Collection Transformer
data CollectionT f a = CollectionT [Err] (f a)
  deriving (Foldable, Functor, Generic, Show, Traversable)

instance Semigroup (f a) => Semigroup (CollectionT f a) where
  CollectionT ms a <> CollectionT ms' b = CollectionT (ms <> ms') (a <> b)
instance Monoid (f a) => Monoid (CollectionT f a) where
  mempty = CollectionT [] mempty

instance e ~ Err => Amend (Either e)
-- | Discards 'Err's
instance e ~ Err => Emit (Either e) where
  emitErr = flip (const id)
instance e ~ Err => Throw (Either e) where
  throwErr = Left
instance e ~ Err => Catch (Either e) where
  catchErr (Left e) f  = f e
  catchErr (Right a) _ = Right a

-- | Discards 'Err's
instance Amend Maybe where
  amendErr = flip (const id)
-- | Discards 'Err's
instance Throw Maybe where
  throwErr = const Nothing
-- | Discards 'Err's
instance Emit Maybe where
  emitErr = flip (const id)

-- | Embeds 'Err's into 'Exception's
instance Amend IO
-- | Embeds 'Err's into 'Exception's
instance Emit IO where
  emitErr a m = putStrLn (show m) *> a
-- | Embeds 'Err's into 'Exception's
instance Throw IO where
  throwErr = throwIO . toException
-- | Embeds 'Err's into 'Exception's
instance Catch IO where
  catchErr a f = IO.catch a $ \e -> maybe (throw e) f (fromException e)

-- | Emit some Loquacious
emit :: (Loquacious e, Emit f, HasCallStack) => f a -> e -> f a
emit a = withFrozenCallStack $ emitErr a . toErr

-- | Throw some Loquacious
throw :: (Loquacious e, Throw f, HasCallStack) => e -> f a
throw = withFrozenCallStack $ throwErr . toErr

-- | Catch some Loquacious
catch :: (Loquacious e, Catch f) => f a -> (e -> f a) -> f a
catch x handle = catchErr x $ \e -> maybe (throwErr e) handle $ fromErr e

-- | Add a note (any 'Loquacious') to a 'throw'n or 'emit'ed 'Err'.
note :: (Amend f, Loquacious t) => t -> f a -> f a
note x = withFrozenCallStack $ flip amendErr $ \(Err xs l) -> Err (toLoq x:xs) l

infixl 0 ?
-- | A flipped version of 'note'.
(?) :: (Amend f, Loquacious t) => f a -> t -> f a
(?) = flip note
