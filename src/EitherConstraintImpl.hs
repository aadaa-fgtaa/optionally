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

class EitherC l r where
  getEitherC :: Either (Dict l) (Dict r)

instance r => EitherC l r where
  getEitherC = Right Dict



eitherC' :: forall l r a . l => (Hold r => r) => (EitherC l r => a) -> a
eitherC' f = case mkEitherC eith of Dict -> f
  where
    r :: Dict r
    r = sub @r undefinedDict

    eith :: Either (Dict l) (Dict r)
    eith = unsafeDupablePerformIO $ catch (evaluate (forceDict r) $> Right r) (\ErrorCall{} -> pure $ Left $ Dict @l)

sub :: (Hold r => r) => Dict (Hold r) -> Dict r
sub Dict = Dict

newtype Gift c a = Gift { unGift :: c => a }

mkEitherC :: Either (Dict l) (Dict r) -> Dict (EitherC l r)
mkEitherC = unsafeCoerce (Gift Dict :: Gift (EitherC l r) (Dict (EitherC l r)))

undefinedDict :: Dict c
undefinedDict = unsafeCoerce (Gift Dict :: Gift c (Dict c)) (errorWithoutStackTrace "")

forceDict :: forall c . Dict c -> ()
forceDict Dict = unGift @c $ unsafeCoerce (`seq` ())



eitherC :: forall l r a . l => (Hold r => r) => (Either (Dict l) (Dict r) -> a) -> a
eitherC f = eitherC' @l @r $ f $ getEitherC @l @r

instanceExist :: forall r . (Hold r => r) => Bool
instanceExist = eitherC @() @r isRight
