{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Err.IO
  ( ErrIO(..)
  ) where

import           Control.Applicative (Alternative(empty, (<|>)), liftA2)
import           Control.Err
import qualified Control.Exception as E
import           Control.Monad.IO.Class (MonadIO(liftIO))

-- | 'IO' 'Monad' wrapper for explicitely tracking error types.
--
-- Terms of the left type are stored in as 'Exception's. In addition to
-- tracking language and CallStack with 'Err's, this enables explicitly
-- tracking of synchronous exceptions.
--
-- Unfortunately, within a @do@ block your exception type is fixed, so that
-- aspect is probably limited in usefulness.
newtype ErrIO e a = ErrIO { runErrIO :: IO a }
  deriving (Monoid, Semigroup)

instance (E.SomeException ~ e) => MonadIO (ErrIO e) where
  liftIO = ErrIO

instance (E.Exception e) => Functor (ErrIO e) where
  fmap f (ErrIO x) = ErrIO $ fmap f x

instance (E.Exception e) => Applicative (ErrIO e) where
  pure = ErrIO . pure
  liftA2 f (ErrIO x) (ErrIO y) = ErrIO $ liftA2 f x y

instance (E.Exception e) => Monad (ErrIO e) where
  (ErrIO x) >>= f = ErrIO $ x >>= runErrIO . f

instance (E.Exception e) => Alternative (ErrIO e) where
  empty = ErrIO . E.throwIO $ toErr @() ()
  ErrIO x <|> ErrIO y = ErrIO $ x `E.catch` (\(_ :: e) -> y)

instance (E.Exception e) => Throw ErrIO e where
  throw = ErrIO . E.throwIO

instance (E.Exception e) => Catch ErrIO e where
  catch (ErrIO x) handle = ErrIO . E.catch x $ runErrIO . handle
