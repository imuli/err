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
import           Control.Exception (Exception, throwIO)
import qualified Control.Exception as IO
import           Control.Monad.IO.Class
import           Data.Default.Class
import           Data.Typeable
import           GHC.Stack
import           Text.Loquate (Loquate(..))
import           Text.Loquate.Doc ((<+>))

-- | Encapsulate an error type with a particular locale or locales.
data Err l = forall t. (HasCallStack, Default l, Loquate l t, Typeable t) => Err t

instance (Default l) => Show (Err l) where
  show = show . loq @l def

instance (Default l, Typeable l) => Exception (Err l)

instance Loquate l (Err l) where
  loq l (Err x) = loq l x <+> "at" <+> loq l (popCallStack callStack)

-- | Convert something to and from an Err.
class (Default l, Typeable l, Loquate l t, Typeable t) => IsErr l t where
  toErr :: t -> Err l
  toErr = Err
  fromErr :: Err l -> Maybe t
  fromErr (Err x) = cast x

instance (Typeable l, Default l) => IsErr l (Err l) where
  toErr = id
  fromErr = Just

instance (Typeable l, Default l) => IsErr l ()

-- | Throwing errors mirrors 'pure' from 'Applicative'.
class Applicative (f (Err l)) => ThrowErr f l where
  err :: (IsErr l t, HasCallStack) => t -> f (Err l) a

-- | Catching errors mirrors '>>=' from 'Monad'.
class (Monad (f (Err l)), ThrowErr f l) => CatchErr f l where
  catchErr :: f (Err l) a -> (Err l -> f (Err l) a) -> f (Err l) a

-- | Catch a particular type of error.
catch :: (IsErr l e, CatchErr f l) => f (Err l) a -> (e -> f (Err l) a) -> f (Err l) a
catch x handle = catchErr x $ \e -> maybe (err e) handle $ fromErr e

-- | 'IO' 'Monad' wrapper for explicitely tracking error types.
--
-- Terms of the left type are stored in as 'Exception's, among other things,
-- this enables explicit tracking of possible (synchronous) exceptions.
newtype ErrIO a b = ErrIO { runErrIO :: IO b }

instance (Exception a) => MonadIO (ErrIO a) where
  liftIO = ErrIO

instance (Exception a) => Functor (ErrIO a) where
  fmap f (ErrIO x) = ErrIO $ fmap f x

instance (Exception a) => Applicative (ErrIO a) where
  pure = ErrIO . pure
  liftA2 f (ErrIO x) (ErrIO y) = ErrIO $ liftA2 f x y

instance (Exception a) => Monad (ErrIO a) where
  (ErrIO x) >>= f = ErrIO $ x >>= runErrIO . f

instance (Exception a) => Alternative (ErrIO a) where
  empty = ErrIO . throwIO $ Err @() ()
  ErrIO x <|> ErrIO y = ErrIO $ x `IO.catch` (\(_ :: a) -> y)

instance (Default l, Typeable l) => ThrowErr ErrIO l where
  err = ErrIO . throwIO . Err @l

instance (Default l, Typeable l) => CatchErr ErrIO l where
  catchErr (ErrIO x) handle = ErrIO . IO.catch x $ runErrIO . handle
