# eitherconstraint

__DISCLAIMER: NEVER USE THIS EXCEPT FOR FUN.__

Union of constrains that tries to satisfy right one and fallbacks to left if failed.

TODO Describe how it works and why you should never use it seriously.

Example:

```haskell
import EitherConstraint

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

main :: IO ()
main = do
  putStrLn $ "instance Foo: " <> show do instanceExist @Foo
  putStrLn $ "instance Bar: " <> show do instanceExist @Bar
  putStrLn $ "instance Fast (): " <> show do instanceExist @(Fast ())
  putStrLn $ "instance Fast Bool: " <> show do instanceExist @(Fast Bool)

  putStrLn $ eitherC_ @(Slow ()) @(Fast ()) (slow ()) (fast ())
  putStrLn $ eitherC_ @(Slow Bool) @(Fast Bool) (slow True) (fast True)
```

Output:

```
instance Foo: True
instance Bar: False
instance Fast (): False
instance Fast Bool: True
slow @()
fast @Bool
```
