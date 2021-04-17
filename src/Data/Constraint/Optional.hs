{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}

module Data.Constraint.Optional (
  Optionally
, type (?)

, maybeC
, orC
, tryC
, isSatisfied

, optionalDict
, give
, discard
, unsafeResolve
) where

import Data.Constraint
import Data.Constraint.Optional.Hold ()
import Data.Constraint.Optional.Impl
import Data.Functor
import Data.Maybe
import Prelude

type f? a = Optionally (f a)

maybeC :: forall c r . Optionally c => r -> (c => r) -> r
maybeC d a = maybe d (\Dict -> a) $ optionalDict @c

orC :: forall c r . Optionally c => (c => r) -> r -> r
orC a b = maybeC @c b a

tryC :: forall c r . Optionally c => (c => r) -> Maybe r
tryC a = optionalDict @c <&> \Dict -> a

isSatisfied :: forall c . Optionally c => Bool
isSatisfied = isJust $ optionalDict @c

give :: forall c r . c => (Optionally c => r) -> r
give = unsafeResolve @c

discard :: forall c r . (Optionally c => r) -> r
discard = unsafeResolve @c
