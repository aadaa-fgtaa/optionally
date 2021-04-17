{-# LANGUAGE UndecidableSuperClasses, UndecidableInstances, AllowAmbiguousTypes, CPP #-}

module Data.Constraint.Optional.Impl where

import {-# SOURCE #-} Data.Constraint.Optional.Hold
import Control.Exception
import Data.Constraint
import Data.Functor
import Data.Maybe
import Prelude
import System.IO.Unsafe
import Unsafe.Coerce

data HoldDict c = (Hold c => c) => HoldDict

class Optionally c where
  optionallyHoldDict :: HoldDict c

instance (Hold c => c) => Optionally c where
  optionallyHoldDict = HoldDict


optionalDict :: forall c . Optionally c => Maybe (Dict c)
optionalDict = unsafeDupablePerformIO $ catch
  do evaluate (forceDict c) $> Just c
  do \NoInstanceError -> pure Nothing
  where
    c :: Dict c
    c | HoldDict <- optionallyHoldDict @c = sub @(Hold c) @c errorDict
-- I'm actually not sure if NOINLINE is really needed, but there is 'errorDict' inside
-- so I want to be sure that ghc wouldn't pass that dictionary somewhere else.
{-# NOINLINE optionalDict #-}


sub :: (a => b) => Dict a -> Dict b
sub Dict = Dict

data NoInstanceError = NoInstanceError
  deriving stock Show
  deriving anyclass Exception

errorDict :: Dict c
errorDict = unsafeCoerce (Gift Dict :: Gift c (Dict c)) (throw NoInstanceError)

newtype Gift c a = Gift { unGift :: c => a }

-- | Not really unsafe but unpredictable. TODO document how exactly.
unsafeResolve :: forall c r . (Hold c => c) => (Optionally c => r) -> r
unsafeResolve f = unsafeCoerce (Gift @(Optionally c) f) (HoldDict @c)

forceDict :: forall c . Dict c -> ()
forceDict Dict = unGift @c $ unsafeCoerce (`seq` ())


-- Overlapping instances to make 'Optionally' opaque

data family Any :: k

type Hold_ a = (Hold a => a) :: Constraint

-- Too lazy to use TH

#define GOCT(constrvars, typevars) \
  instance {-# OVERLAPPING #-} (Hold_ (constr (typ Any typevars) constrvars)) => Optionally (constr (typ Any typevars) constrvars) where { optionallyHoldDict = HoldDict };

#define GOC(constrvars) \
  GOCT(constrvars, ) \
  GOCT(constrvars, t1) \
  GOCT(constrvars, t1 t2) \
  GOCT(constrvars, t1 t2 t3) \
  GOCT(constrvars, t1 t2 t3 t4) \
  GOCT(constrvars, t1 t2 t3 t4 t5) \
  GOCT(constrvars, t1 t2 t3 t4 t5 t6) \
  GOCT(constrvars, t1 t2 t3 t4 t5 t6 t7) \
  GOCT(constrvars, t1 t2 t3 t4 t5 t6 t7 t8) \
  GOCT(constrvars, t1 t2 t3 t4 t5 t6 t7 t8 t9) \
  GOCT(constrvars, t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)

GOC()
GOC(c1)
GOC(c1 c2)
GOC(c1 c2 c3)
GOC(c1 c2 c3 c4)
GOC(c1 c2 c3 c4 c5)
GOC(c1 c2 c3 c4 c5 c6)
GOC(c1 c2 c3 c4 c5 c6 c7)
GOC(c1 c2 c3 c4 c5 c6 c7 c8)
GOC(c1 c2 c3 c4 c5 c6 c7 c8 c9)
GOC(c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)

#undef GOCT
#undef GOC
