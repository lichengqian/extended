module Extended.Data.Word (
    module Data.Word
  , module Data.DoubleWord
  , word160ToBytes
  , word256ToBytes
    ) where

import           Data.Bits
import           Data.DoubleWord
import           Data.Word

word256ToBytes::Word256->[Word8]
word256ToBytes x =
     map (\byte -> fromIntegral $ (x `shiftR` (byte*8)) .&. 0xFF) [31,30..0]

word160ToBytes::Word160->[Word8]
word160ToBytes x =
     map (\byte -> fromIntegral $ (x `shiftR` (byte*8)) .&. 0xFF) [19,18..0]

