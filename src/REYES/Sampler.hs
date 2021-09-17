{-# LANGUAGE BangPatterns #-}

module REYES.Sampler where

import Data.Bits (shiftR, xor, (.&.), (.|.))
import Data.Word (Word32)

-- | Pseudorandom permutation function.
--
-- The permutation function @'permute' index size pat@ chooses a random integer
-- from the range @[0, size-1]@. @index@ specifies which integer to choose, such
-- that each integer is chosen only once. @pat@ specifies which random pattern
-- to draw from, such that each value of @pat@ should produce a different random
-- sequence.
--
-- For example, we may compute a full random permutation of the sequence
-- @[0,1,2,3,4]@ by drawing the permutation at each index as follows:
--
-- >>> let size = 5  -- 5 elements, 0 to 4
-- >>> let pat = 0
-- >>> (\i -> permute i size pat) <$> [0 .. size-1]
-- [0,4,2,3,1]
--
-- Each @pat@ value selects a different permutation pattern:
--
-- >>> let pat = 1
-- >>> (\i -> permute i size pat) <$> [0 .. size-1]
-- [0,1,2,4,3]
--
-- This is Listing 3 from Kensler, 2013:
--
--   * Kensler, A (2013) Correlated Multi-Jittered Sampling. Pixar Technical
--     Memo, 13-01.
permute ::
  -- | Index into the permutation. This must be a value in the range
  --   @[0, size-1]@.
  Word32 ->
  -- | Length or size of the permutation. How many elements it has.
  Word32 ->
  -- | Pattern number. The instance of the permutation from which we are
  --   drawing elements.
  Word32 ->
  -- | Permuted value.
  Word32
permute !index !size !pat =
  let orEq :: (Word32 -> Word32) -> Word32 -> Word32
      orEq !f !x = x .|. f x

      xorEq :: (Word32 -> Word32) -> Word32 -> Word32
      xorEq !f !x = x `xor` f x

      -- flip (.) so that operations read in the same direction as the original
      -- C code
      (.>) :: (a -> b) -> (b -> c) -> a -> c
      (.>) = flip (.)

      w :: Word32
      w =
        orEq (`shiftR` 1)
          .> orEq (`shiftR` 2)
          .> orEq (`shiftR` 4)
          .> orEq (`shiftR` 8)
          .> orEq (`shiftR` 16)
          $ size - 1

      cycleWalk :: Word32 -> Word32
      cycleWalk !q =
        let r :: Word32
            r =
              xor pat
                .> (*) 0xe170893d
                .> xor (pat `shiftR` 16)
                .> xorEq ((.&.) w .> (`shiftR` 4))
                .> xor (pat `shiftR` 8)
                .> (*) 0x0929eb3f
                .> xor (pat `shiftR` 23)
                .> xorEq ((.&.) w .> (`shiftR` 1))
                .> (*) (1 .|. (pat `shiftR` 27))
                .> (*) 0x6935fa69
                .> xorEq ((.&.) w .> (`shiftR` 11))
                .> (*) 0x74dcb303
                .> xorEq ((.&.) w .> (`shiftR` 2))
                .> (*) 0x9e501cc3
                .> xorEq ((.&.) w .> (`shiftR` 2))
                .> (*) 0xc860a3df
                .> (.&.) w
                .> xorEq (`shiftR` 5)
                $ q
         in if r >= size then cycleWalk r else r
   in (cycleWalk index + pat) `mod` size
