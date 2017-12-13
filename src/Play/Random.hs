module Play.Random
  ( initRandomGen
  , randomValue
  ) where

import qualified Data.Vector as Vector

import           Play.Types

import           System.Random.MWC (GenIO)
import qualified System.Random.MWC as Random


-- | Initialize a random generator with the given seed if provided. If no seed
-- is provided, the generator will silenty user a seed obtained from /dev/urandom.
initRandomGen :: Maybe SeedValue -> IO GenIO
initRandomGen msv =
  case msv of
    Nothing -> Random.createSystemRandom
    Just (SeedValue v) -> Random.initialize $ Vector.singleton v

-- According to the specs for random-mwc, if `uniformR` generates floating point
-- numbers, it will generate numbers in the range (a,b] if one ignores rounding
-- errors.
-- This meets the spec.
randomValue :: GenIO -> IO Double
randomValue = Random.uniformR (0.0, 1.0)

