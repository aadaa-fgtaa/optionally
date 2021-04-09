{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module EitherConstraint (
  EitherC ( getEitherC )
, eitherC'
, eitherC
, instanceExist
, module Data.Constraint
, Hold )
where

import EitherConstraintImpl
import Data.Constraint

class c => Hold c
instance c => Hold c
