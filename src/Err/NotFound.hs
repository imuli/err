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
   Description : The NotFound Err
-}
module Err.NotFound
  ( NotFound(..)
  ) where

import           GHC.Generics (Generic)
import           Text.Loquate (Loquate(loq))
import           Text.Loquate.Doc ((<+>))

-- | Error type for failed lookups of all kinds
newtype NotFound t = NotFound t
  deriving (Eq, Functor, Generic, Ord, Read, Show)

instance Loquate l t => Loquate l (NotFound t) where
  loq l (NotFound x) = "NotFound" <+> loq l x
