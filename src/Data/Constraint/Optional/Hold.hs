{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Constraint.Optional.Hold ( Hold ) where

import Data.Constraint.Optional.Impl ()  -- Suppress warning about unnecessary SOURCE-import

class c => Hold c
instance c => Hold c
