module Play.Types
  ( SendSeconds (..)
  , WaitSeconds (..)
  , SeedValue (..)
  ) where

import Data.Word (Word32)

-- Chose Word32 for these values because negatve numbers don't really make sense.
newtype SendSeconds
  = SendSeconds Word32
  deriving (Eq, Show)

newtype WaitSeconds
  = WaitSeconds Word32
  deriving (Eq, Show)

-- The random-mwc package uses a vector of Word32 for seeds. We will just uses
-- a single value.
newtype SeedValue
  = SeedValue Word32
  deriving (Eq, Show)
