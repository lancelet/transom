{-# LANGUAGE BangPatterns #-}

module REYES.Sampler where

import Data.Bits (shiftR, xor, (.&.), (.|.))
import Data.Word (Word32)

-- | Pseudorandom permutation function.
--
-- This is Listing 3 from Kensler, 2013.
permute ::
  -- | Index into the permutation.
  Word32 ->
  -- | Length of the permutation.
  Word32 ->
  -- | Permutation number.
  Word32 ->
  -- | Permuted value.
  Word32
permute !i !l !p =
  let w1, w2, w3, w4, w5, w6, w :: Word32
      -- uint32_t w = l - 1;
      w1 = l - 1
      -- w |= w >> 1;
      w2 = w1 .|. (w1 `shiftR` 1)
      -- w |= w >> 2;
      w3 = w2 .|. (w2 `shiftR` 2)
      -- w |= w >> 4;
      w4 = w3 .|. (w3 `shiftR` 4)
      -- w |= w >> 8;
      w5 = w4 .|. (w4 `shiftR` 8)
      -- w |= w >> 16;
      w6 = w5 .|. (w5 `shiftR` 16)
      w = w6

      -- do {
      go :: Word32 -> Word32
      go !i1 =
        let -- i ^= p;
            i2 = i1 `xor` p
            -- i *= 0xe170893d;
            i3 = i2 * 0xe170893d
            -- i ^= p >> 16;
            i4 = i3 `xor` (p `shiftR` 16)
            -- i ^= (i & w) >>  4;
            i5 = i4 `xor` ((i4 .&. w) `shiftR` 4)
            -- i ^= p >> 8;
            i6 = i5 `xor` (p `shiftR` 8)
            -- i *= 0x0929eb3f;
            i7 = i6 * 0x0929eb3f
            -- i ^= p >> 23;
            i8 = i7 `xor` (p `shiftR` 23)
            -- i ^= (i & w) >> 1;
            i9 = i8 `xor` ((i8 .&. w) `shiftR` 1)
            -- i *= 1 | p >> 27;
            i10 = i9 * (1 .|. (p `shiftR` 27))
            -- i *= 0x6935fa69;
            i11 = i10 * 0x6935fa69
            -- i ^= (i & w) >> 11;
            i12 = i11 `xor` ((i11 .&. w) `shiftR` 11)
            -- i *= 0x74dcb303;
            i13 = i12 * 0x74dcb303
            -- i ^= (i & w) >> 2;
            i14 = i13 `xor` ((i13 .&. w) `shiftR` 2)
            -- i *= 0x9e501cc3;
            i15 = i14 * 0x9e501cc3
            -- i ^= (i & w) >> 2;
            i16 = i15 `xor` ((i15 .&. w) `shiftR` 2)
            -- i *= 0xc860a3df;
            i17 = i16 * 0xc860a3df
            -- i &= w;
            i18 = i17 .&. w
            -- i ^= i >> 5;
            i19 = i18 `xor` (i18 `shiftR` 5)
         in -- } while (i >= l);
            if i19 >= l then go i19 else i19

      i' = go i
   in -- return (i + p) % l;
      (i' + p) `mod` l
