{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module EitherConstraint (
  eitherC
, eitherC'
, eitherC_
, instanceExist
, module Data.Constraint
, Hold )
where

import EitherConstraintImpl
import Data.Constraint

class c => Hold c
instance c => Hold c
