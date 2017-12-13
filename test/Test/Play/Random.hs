{-# LANGUAGE TemplateHaskell #-}
module Test.Play.Random
  ( tests
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Play

-- Test that two generators seeded with the same seed generate the same first value.
prop_generator_with_seed :: Property
prop_generator_with_seed =
  H.property $ do
    seed <- Just . SeedValue <$> H.forAll (Gen.integral (Range.linear 0 0xffffffff))
    val1 <- liftIO (randomValue =<< initRandomGen seed)
    val2 <- liftIO (randomValue =<< initRandomGen seed)
    val1 === val2

-- Test that two generators seeded *without* a seed generate a different first value.
-- This has a roughly 1 in 2^52 chance of failing due to two generated values colliding.
prop_generator_without_seed :: Property
prop_generator_without_seed =
  H.property $ do
    val1 <- liftIO (randomValue =<< initRandomGen Nothing)
    val2 <- liftIO (randomValue =<< initRandomGen Nothing)
    H.assert $ val1 /= val2

-- Test that the first and second value from the same generator are different.
-- This has a roughly 1 in 2^52 chance of failing due to two generated values colliding.
prop_values_are_random :: Property
prop_values_are_random =
  H.property $ do
    (val1, val2) <- liftIO $ do
        gen <- initRandomGen Nothing
        (,) <$> randomValue gen <*> randomValue gen
    H.assert $ val1 /= val2


tests :: IO Bool
tests = H.checkParallel $$(H.discover)
