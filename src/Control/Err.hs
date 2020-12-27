{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{- Module      : Control.Err
   Copyright   : Public Domain
   Description : Locale Sensitive Error Messages
-}
module Control.Err where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Default.Class
import           Data.Typeable
import           GHC.Stack
import           Text.Loquate (Loquate(..))
import           Text.Loquate.Doc ((<+>))

-- | Encapsulate an error type with a particular locale or locales.
data Err l = forall t. (HasCallStack, Default l, Loquate l t, Typeable t) => Err t

instance Typeable l => Loquate l (Err l) where
  loq l (Err x) = loq l x <+> "at" <+> loq l (popCallStack callStack)

instance (Default l, Typeable l) => Show (Err l) where
  show = show . loq @l def

-- | Throwing errors mirrors 'pure' from 'Applicative'.
class Applicative (f (Err l)) => ThrowErr f l where
  err :: (Typeable t, Loquate l t, HasCallStack) => t -> f (Err l) a

-- | Catching errors mirrors '>>=' from 'Monad'.
class (Monad (f (Err l)), ThrowErr f l) => CatchErr f l where
  catch :: (Typeable e, Loquate l e) => f (Err l) a -> (e -> f (Err l) a) -> f (Err l) a

instance (Default l, Typeable l) => Exception (Err l)

-- | 'IO' 'Monad' for 'Err's
--
-- Terms of the left type are stored in as 'Exception's, among other things,
-- this enables explicit tracking of possible (synchronous) exceptions.
newtype IOE a b = IOE { runIOE :: IO b }

instance (Exception a) => MonadIO (IOE a) where
  liftIO = IOE

instance (Exception a) => Functor (IOE a) where
  fmap f (IOE x) = IOE $ fmap f x

instance (Exception a) => Applicative (IOE a) where
  pure = IOE . pure
  liftA2 f (IOE x) (IOE y) = IOE $ liftA2 f x y

instance (Exception a) => Monad (IOE a) where
  (IOE x) >>= f = IOE $ x >>= runIOE . f

instance (Default l, Typeable l) => ThrowErr IOE l where
  err = IOE . throwIO . Err @l

test :: (Default l, Typeable l, ThrowErr f l) => f (Err l) Int
test = err (1 :: Int)

  {-
-- | @SomeErr@ encapsulates an loquated error, along with a collection of notes
-- about the error.
data SomeErr l = forall t. Err l t => SomeErr [SomeErr l] t

instance (Typeable l, l ~ l') => Loquacious l (SomeErr l') where
  loq (SomeErr xs x) = foldMap loq xs <> loq @l x

instance Typeable l => Show (SomeErr l) where
  show = show . loq

instance Typeable l => IO.Exception (SomeErr l)

-- | An 'Err'or Type Hierarchy, much like 'Exception' and 'SomeException', but
-- with more customizable rendering, and easy annotation.
class Loquacious l t => Err l t where
  toErr :: t -> SomeErr l
  toErr = SomeErr []
  fromErr :: SomeErr l -> Maybe t
  fromErr (SomeErr _ x) = cast x

instance Typeable l => Err l (SomeErr l) where
  toErr = id
  fromErr = Just

note :: Err l t => t -> SomeErr l -> SomeErr l
note n (SomeErr ns x) = SomeErr (toErr n : ns) x

(%%) :: forall l f t a . (Err l t, AltCatch l f) => f a -> t -> f a
a %% b = a `catchSome` (errSome @l . note b)

-- | Much like 'Plus' from semigroupoids, but instead of @zero@ we have
-- @err@, which holds some error type.
class Alt f => AltErr l f where
  errSome :: SomeErr l -> f a

err :: forall l t f a . (AltErr l f, Err l t) => t -> f a
err = errSome @l . toErr

-- | Much like 'MonadCatch' from the exceptions package.
--
--
class AltErr l f => AltCatch l f where
  catchSome :: f a -> (SomeErr l -> f a) -> f a

catch :: forall l t f a . (AltCatch l f, Err l t) => f a -> (t -> f a) -> f a
catch a f = catchSome @l a $ \e -> maybe (errSome e) f (fromErr e)

-- | Simply discard error information.
instance AltErr l Maybe where
  errSome _ = Nothing

instance SomeErr l ~ e => AltErr l (Either e) where
  errSome = Left

instance SomeErr l ~ e => AltCatch l (Either e) where
  catchSome (Left e) f = f e
  catchSome r _        = r

-- | The alternative implementation here catches all synchronous exceptions.
instance Typeable l => AltErr l IO where
  errSome = IO.throwIO

instance Typeable l => AltCatch l IO where
  catchSome = IO.catch

data NotFound t = NotFound t
  deriving (Eq, Ord, Read, Show)

instance Loquacious l t => Err l (NotFound t)

instance Loquacious l t => Loquacious l (NotFound t) where
  loq (NotFound t) = loq @l t <> " was not found."
-}
