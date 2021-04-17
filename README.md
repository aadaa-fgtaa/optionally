# optionally

__DISCLAIMER: NEVER USE THIS EXCEPT FOR FUN.__

Require constraints `Optionally`. See [this blog
post](https://aadaa-fgtaa.github.io/blog/optionally/) for implementation details and limitations.

__NOTE__ Works only with optimisations enabled so don't forget to pass `-fobject-code -O` if you
want to test it in ghci.

Example:

```haskell
class Foo
instance Foo

class Bar

class Slow a where
  slow :: a -> String
class Fast a where
  fast :: a -> String

instance Slow () where
  slow _ = "slow @()"

instance Slow Bool where
  slow _ = "slow @Bool"
instance Fast Bool where
  fast _ = "fast @Bool"

class Baz (a :: k)

instance Baz 1
instance Baz 'True

tryShow :: forall a . Show? a => a -> Maybe String
tryShow a = tryC @(Show a) $ show a

fastOrSlow :: forall a . (Slow a, Fast? a) => a -> String
fastOrSlow = orC @(Fast a) fast slow

nubSmart :: forall a . (Eq a, Ord? a) => [a] -> [a]
nubSmart a = orC @(Ord a) (trace "nubOrd" $ nubOrd a) (trace "nubEq" $ nub a)

nubForceEq :: forall a . Eq a => [a] -> [a]
nubForceEq = discard @(Ord a) nubSmart

nubForceOrd :: forall a . Ord a => [a] -> [a]
nubForceOrd = give @(Ord a) nubSmart

newtype NoOrd a = NoOrd a
  deriving stock (Show, Eq)
```

```
isSatisfied @Foo: True
isSatisfied @Bar: False
isSatisfied @(Baz 1): True
isSatisfied @(Baz 2): False
isSatisfied @(Baz 'True): True
isSatisfied @(Baz 'False): False
isSatisfied @(Show Int): True
isSatisfied @(Show [Int]): True
isSatisfied @(Show [Int -> Int]): False
isSatisfied @(Monad IO): True
isSatisfied @(MonadReader Int IO): False
isSatisfied @(MonadReader Int (ReaderT Int IO)): True

tryShow True: Just "True"
tryShow $ id @Bool: Nothing
tryShow (True, ()): Just "(True,())"
tryShow (True, id @Bool): Nothing

fastOrSlow (): "slow @()"
fastOrSlow True: "fast @Bool"

-- traces are printed too early but you got the idea

nubOrd
nubSmart [1, 2 :: Int]: [1,2]
nubEq
nubSmart [NoOrd 1, NoOrd 2 :: NoOrd Int]: [NoOrd 1,NoOrd 2]
nubOrd
nubForceOrd [1, 2 :: Int]: [1,2]
nubEq
nubForceEq [NoOrd 1, NoOrd 2 :: NoOrd Int]: [NoOrd 1,NoOrd 2]
```
