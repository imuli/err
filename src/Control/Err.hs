{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{- Module      : Control.Err
   Copyright   : Public Domain
   Description : Locale Sensitive Error Messages
-}
module Control.Err
  ( -- Exception type tracking
    Throw(..)
  , Catch(..)
    -- 'Err's, loquatable exceptions.
  , Err
  , toErr
  , fromErr
    -- Throwing and Catching 'Err's
  , throwErr
  , catchErr
  , note
  , (?)
  , ThrowE
  , CatchE
  ) where

import           Control.Applicative (Alternative(empty))
import           Control.Exception (Exception)
import           Data.Default.Class (Default(def))
import           Data.Maybe (catMaybes)
import           Data.Typeable (cast)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack, callStack, getCallStack, withFrozenCallStack)
import           Text.Loquate (Lang, Loq(Loq), Loquate(loq))
import           Text.Loquate.Doc (align, nest, sep, softline, vsep, (<+>))

-- | Encapsulate an error type with a particular locale or locales, with a list
-- of information about it.
data Err l = forall t. (HasCallStack, Loquate l t) => Err [Loq l] t

instance (Lang l) => Show (Err l) where
  show = show . loq @l def

instance (Lang l) => Exception (Err l)

instance (Lang l, l ~ l') => Loquate l' (Err l) where
  loq l (Err ms x) = loq l x <> nest 2 (softline <> align (vsep $ catMaybes [stack, notes]))
    where
      stack | null (getCallStack callStack) = Nothing
            | otherwise = Just $ "@" <+> loq l callStack
      notes | null ms = Nothing
            | otherwise = Just $ "?" <+> (align . sep $ loq l <$> ms)

toErr :: (HasCallStack, Loquate l t) => t -> Err l
toErr = withFrozenCallStack $ Err []

fromErr :: Loquate l t => Err l -> Maybe t
fromErr (Err _ x) = cast x

-- | Throwing errors mirrors 'pure' from 'Applicative', whilst transforming
-- them mirrors 'fmap'.
class Applicative (f e) => Throw f e where
  -- | Like 'pure', for the first type argument.
  throw :: HasCallStack => e -> f e a
  -- | Like 'fmap' for the first type argument.
  amend :: Throw f e' => f e a -> (e -> e') -> f e' a
  default amend :: (Catch f e, Throw f e') => f e a -> (e -> e') -> f e' a
  amend a f = catch a (throw . f)

-- | Catching errors mirrors '>>=' from 'Monad'.
class (Monad (f e), Throw f e) => Catch f e where
  catch :: Throw f e' => f e a -> (e -> f e' a) -> f e' a

instance Throw Either e where
  throw = Left
instance Catch Either e where
  catch (Left e) f  = f e
  catch (Right a) _ = Right a

-- | Wrap an instance of 'Alternative' with an extra type argument to discard
-- all 'throw'n errors, instead returning 'empty'.
newtype ThrowT f e a = ThrowT { runThrowT :: f a }
  deriving
     ( Alternative
     , Applicative
     , Eq
     , Foldable
     , Functor
     , Generic
     , Monad
     , MonadFail
     , Monoid
     , Ord
     , Read
     , Semigroup
     , Show
     , Traversable
     )

instance Alternative f => Throw (ThrowT f) e where
  throw _ = ThrowT empty
  amend (ThrowT x) _ = ThrowT x

type ThrowE f l a = Throw f (Err l) => f (Err l) a
type CatchE f l a = Catch f (Err l) => f (Err l) a

-- | Throw some 'Err'or in a particular language.
throwErr :: forall l e a f. (Throw f (Err l), Loquate l e, HasCallStack) => e -> f (Err l) a
throwErr = withFrozenCallStack $ throw . toErr @l

-- | Catch errors in a particular language.
catchErr :: forall l e a f. (Loquate l e, Catch f (Err l))
         => f (Err l) a -> (e -> f (Err l) a) -> f (Err l) a
catchErr x handle = catch x $ \e -> maybe (throw e) handle $ fromErr e

-- | Add a note (any item that may be loquated) to a 'throw'n 'Err'.
note :: forall l a f t. (Throw f (Err l), Loquate l t)
     => t -> f (Err l) a -> f (Err l) a
note x = withFrozenCallStack $ flip amend $ \(Err xs l) -> Err (Loq x:xs) l

infixl 0 ?
-- | A flipped version of 'note'.
(?) :: forall t l a f. (Throw f (Err l), Loquate l t)
    => f (Err l) a -> t -> f (Err l) a
(?) = flip note
