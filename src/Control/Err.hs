{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{- Module      : Control.Err
   Description : Locale Sensitive Error Messages
-}
module Control.Err where

import qualified Control.Exception as IO
import           Text.Loquacious
import           Data.Typeable

-- | @SomeErr@ encapsulates an loquated error, along with a collection of notes
-- about the error.
data SomeErr l = forall t. Err l t => SomeErr [SomeErr l] t

instance (Typeable l, l ~ l') => Loquacious l (SomeErr l') where
  loq (SomeErr xs x) = foldMap loq xs <> loq @l x

instance Typeable l => Show (SomeErr l) where
  show = show . loq

instance Typeable l => IO.Exception (SomeErr l)

-- | An 'Err'or Type Hierarchy, much like 'Exception' and 'SomeException', but
-- with more customizable rendering.
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

(%%) :: forall l f t a . (Err l t, AppCatch l f) => f a -> t -> f a
a %% b = a `catchSome` (errSome @l . note b)

-- | Much like 'Control.Applicative.Alternative', but instead of @empty@ we have
-- @err@, which provides an error message.
class Applicative f => AppErr l f where
  errSome :: SomeErr l -> f a
  (<!>) :: f a -> f a -> f a

err :: forall l t f a . (AppErr l f, Err l t) => t -> f a
err = errSome @l . toErr

-- | Much like 'MonadCatch' from the exceptions package.
class AppErr l f => AppCatch l f where
  catchSome :: f a -> (SomeErr l -> f a) -> f a

catch :: forall l t f a . (AppCatch l f, Err l t) => f a -> (t -> f a) -> f a
catch a f = catchSome @l a $ \e -> maybe (errSome e) f (fromErr e)

-- | Simply discard error information.
instance AppErr l Maybe where
  errSome _ = Nothing
  Nothing <!> b = b
  a <!> _       = a

instance SomeErr l ~ e => AppErr l (Either e) where
  errSome = Left
  Left _ <!> b = b
  a <!> _      = a

instance SomeErr l ~ e => AppCatch l (Either e) where
  catchSome (Left e) f = f e
  catchSome r _        = r

-- | The alternative implementation here catches all synchronous exceptions.
instance Typeable l => AppErr l IO where
  errSome = IO.throwIO
  a <!> b = IO.catches a [ IO.Handler $ \(e :: IO.SomeAsyncException) -> IO.throw e
                         , IO.Handler $ \(_ :: IO.SomeException) -> b
                         ]

instance Typeable l => AppCatch l IO where
  catchSome = IO.catch

data NotFound t = NotFound t
  deriving (Eq, Ord, Read, Show)

instance Loquacious l t => Err l (NotFound t)

instance Loquacious l t => Loquacious l (NotFound t) where
  loq (NotFound t) = loq @l t <> " was not found."
