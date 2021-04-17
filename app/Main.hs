{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Control.Monad.Reader
import Data.Constraint.Optional
import Data.Containers.ListUtils
import Data.List
import Prelude
import Debug.Trace

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

main :: IO ()
main = do
  putStrLn $ "isSatisfied @Foo: "                                <> show do isSatisfied @Foo
  putStrLn $ "isSatisfied @Bar: "                                <> show do isSatisfied @Bar
  putStrLn $ "isSatisfied @(Baz 1): "                            <> show do isSatisfied @(Baz 1)
  putStrLn $ "isSatisfied @(Baz 2): "                            <> show do isSatisfied @(Baz 2)
  putStrLn $ "isSatisfied @(Baz 'True): "                        <> show do isSatisfied @(Baz 'True)
  putStrLn $ "isSatisfied @(Baz 'False): "                       <> show do isSatisfied @(Baz 'False)
  putStrLn $ "isSatisfied @(Show Int): "                         <> show do isSatisfied @(Show Int)
  putStrLn $ "isSatisfied @(Show [Int]): "                       <> show do isSatisfied @(Show [Int])
  putStrLn $ "isSatisfied @(Show [Int -> Int]): "                <> show do isSatisfied @(Show [Int -> Int])
  putStrLn $ "isSatisfied @(Monad IO): "                         <> show do isSatisfied @(Monad IO)
  putStrLn $ "isSatisfied @(MonadReader Int IO): "               <> show do isSatisfied @(MonadReader Int IO)
  putStrLn $ "isSatisfied @(MonadReader Int (ReaderT Int IO)): " <> show do isSatisfied @(MonadReader Int (ReaderT Int IO))

  putStrLn ""

  putStrLn $ "tryShow True: "             <> show do tryShow True
  putStrLn $ "tryShow $ id @Bool: "       <> show do tryShow $ id @Bool
  putStrLn $ "tryShow (True, ()): "       <> show do tryShow (True, ())
  putStrLn $ "tryShow (True, id @Bool): " <> show do tryShow (True, id @Bool)

  putStrLn ""

  putStrLn $ "fastOrSlow (): "   <> show do fastOrSlow ()
  putStrLn $ "fastOrSlow True: " <> show do fastOrSlow True

  putStrLn "\n-- traces are printed too early but you got the idea\n"

  putStrLn $ "nubSmart [1, 2 :: Int]: "                     <> show do nubSmart [1, 2 :: Int]
  putStrLn $ "nubSmart [NoOrd 1, NoOrd 2 :: NoOrd Int]: "   <> show do nubSmart [NoOrd 1, NoOrd 2 :: NoOrd Int]
  putStrLn $ "nubForceOrd [1, 2 :: Int]: "                  <> show do nubForceOrd [1, 2 :: Int]
  putStrLn $ "nubForceEq [NoOrd 1, NoOrd 2 :: NoOrd Int]: " <> show do nubForceEq [NoOrd 1, NoOrd 2 :: NoOrd Int]
