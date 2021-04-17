module Data.Constraint.Optional.Hold where

import Data.Kind ( Constraint )

class Hold (c :: Constraint)
