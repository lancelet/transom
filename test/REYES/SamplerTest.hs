{-# LANGUAGE OverloadedStrings #-}

module REYES.SamplerTest (tests) where

import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Data.Word (Word32)
import Hedgehog (Gen, Group (Group), Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified REYES.Sampler as Sampler

tests :: Group
tests =
  Group
    "REYES.Sampler"
    [ ("permute matches C implementation", prop_permute_vs_C),
      ("permute must be complete", prop_permute_complete)
    ]

-- | All values in a 'Sampler.permute' sequence must be present.
prop_permute_complete :: Property
prop_permute_complete = property $ do
  l <- forAll $ Gen.integral (Range.linear 1 (128 ^ 2))
  p <- forAll $ Gen.integral (Range.linear minBound maxBound)

  let orderedSeq :: [Word32]
      orderedSeq = [0 .. (l - 1)]

      permutedSeq :: [Word32]
      permutedSeq = (\i -> Sampler.permute i l p) <$> orderedSeq

      sortedPermutation :: [Word32]
      sortedPermutation = sort permutedSeq

  sortedPermutation === orderedSeq

-- | Check that the 'Sampler.permute' function matches a C implementation.
--
-- The C implementation is from Listing 3 of:
--
--   * Kensler, A (2013) Correlated Multi-Jittered Sampling. Pixar Technical
--     Memo, 13-01.
prop_permute_vs_C :: Property
prop_permute_vs_C = property $ do
  l <- forAll $ Gen.integral (Range.linear 1 maxBound)
  i <- forAll $ Gen.integral (Range.linear 0 (l - 1))
  p <- forAll $ Gen.integral (Range.linear minBound maxBound)
  result_c <- liftIO $ permute_c i l p
  let result_hs :: Word32
      result_hs = Sampler.permute i l p
  result_hs === result_c

-- | The original @permute@ function from Kensler (2013):
--
--   * Kensler, A (2013) Correlated Multi-Jittered Sampling. Pixar Technical
--     Memo, 13-01.
foreign import ccall "permute"
  permute_c :: Word32 -> Word32 -> Word32 -> IO Word32
