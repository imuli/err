{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Err.NotFound
Copyright   : Public Domain
Description : The NotFound Err
Stability   : experimental
-}

module Err.NotFound
  ( NotFound(..)
  , maybeNF
  , headNF
  , findNF
  , filterNF
  ) where

import           Control.Err (Throw, throw)
import           Data.Foldable (find)
import           Data.Text.Prettyprint.Doc ((<+>))
import           GHC.Generics (Generic)
import           Text.Loquacious (Loquacious(fromLoq, loq, toLoq), fromLoqTraversable, toLoqFunctor)

-- | Error type for failed lookups of all kinds
newtype NotFound t = NotFound t
  deriving (Eq, Foldable, Functor, Generic, Ord, Read, Show, Traversable)

instance Loquacious t => Loquacious (NotFound t) where
  loq l (NotFound x) = "NotFound" <+> l x
  toLoq = toLoqFunctor
  fromLoq = fromLoqTraversable

-- | extract the contents of a maybe, throwing 'NotFound' on 'Nothing'
maybeNF :: (Loquacious x, Applicative t, Throw t) => x -> Maybe a -> t a
maybeNF m = maybe (throw $ NotFound m) pure

-- | take the head of a list, throwing 'NotFound' for an empty list
headNF :: (Loquacious x, Applicative t, Throw t) => x -> [a] -> t a
headNF _ (x:_) = pure x
headNF m []    = throw (NotFound m)

-- | extract the first value matching a predicate from some container, throwing
-- 'NotFound' if there is none
findNF :: (Loquacious x, Foldable f, Applicative t, Throw t) => x -> (a -> Bool) -> f a -> t a
findNF m p = maybeNF m . find p

-- | check whether some value in a monad meets a predicate, throwing 'NotFound' if it doesn't
filterNF :: (Loquacious x, Monad t, Throw t) => x -> (a -> Bool) -> t a -> t a
filterNF m p = (=<<) $ \x -> if p x then pure x else throw (NotFound m)
