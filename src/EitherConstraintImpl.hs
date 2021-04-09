{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EitherConstraintImpl where

import {-# SOURCE #-} EitherConstraint
import Control.Exception
import Data.Constraint
import Data.Either
import Data.Functor
import Prelude
import System.IO.Unsafe
import Unsafe.Coerce


eitherC :: forall l r a . (l, Hold r => r) => (Either (Dict l) (Dict r) -> a) -> a
eitherC f = f eith
  where
    r :: Dict r
    r = sub @r undefinedDict

    eith :: Either (Dict l) (Dict r)
    eith = unsafeDupablePerformIO $ catch (evaluate (forceDict r) $> Right r) (\ErrorCall{} -> pure $ Left $ Dict @l)

sub :: (Hold r => r) => Dict (Hold r) -> Dict r
sub Dict = Dict

newtype Gift c a = Gift { unGift :: c => a }

undefinedDict :: Dict c
undefinedDict = unsafeCoerce (Gift Dict :: Gift c (Dict c)) (errorWithoutStackTrace "")

forceDict :: forall c . Dict c -> ()
forceDict Dict = unGift @c $ unsafeCoerce (`seq` ())



eitherC' :: (l, Hold r => r) => Either (Dict l) (Dict r)
eitherC' = eitherC id

eitherC_ :: forall l r a . (l, Hold r => r) => (l => a) -> (r => a) -> a
eitherC_ f g = eitherC @l @r $ either (\Dict -> f) (\Dict -> g)

instanceExist :: forall r . (Hold r => r) => Bool
instanceExist = eitherC @() @r isRight
