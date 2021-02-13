{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{- Module      : Err.NotFound
   Copyright   : Public Domain
   Description : The NotFound Err
-}
module Err.NotFound
  ( NotFound(..)
  , maybeNF
  , headNF
  , findNF
  , filterNF
  ) where

import           Control.Err
import           Data.Foldable (find)
import           GHC.Generics (Generic)
import           Text.Loquate (Loquate(loq))
import           Text.Loquate.Doc ((<+>))

-- | Error type for failed lookups of all kinds
newtype NotFound t = NotFound t
  deriving (Eq, Foldable, Functor, Generic, Ord, Read, Show, Traversable)

instance Loquate l t => Loquate l (NotFound t) where
  loq l (NotFound x) = "NotFound" <+> loq l x

-- | extract the contents of a maybe, throwing 'NotFound' on 'Nothing'
maybeNF :: Loquate l t => t -> Maybe a -> ThrowE m l a
maybeNF m = maybe (throwErr $ NotFound m) pure

-- | take the head of a list, throwing 'NotFound' for an empty list
headNF :: Loquate l t => t -> [a] -> ThrowE m l a
headNF _ (x:_) = pure x
headNF m []    = throwErr (NotFound m)

-- | extract the first value matching a predicate from some container, throwing
-- 'NotFound' if there is none
findNF :: (Loquate l t, Foldable f) => t -> (a -> Bool) -> f a -> ThrowE m l a
findNF m p = maybeNF m . find p

-- | check whether some value in a monad meets a predicate, throwing 'NotFound' if it doesn't
filterNF :: (Loquate l t, Monad (m (Err l))) => t -> (a -> Bool) -> ThrowE m l a -> ThrowE m l a
filterNF m p = (=<<) $ \x -> if p x then pure x else throwErr (NotFound m)
