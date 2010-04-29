module Codec.Hex
  ( intToHex
  , int8ToHex
  , int16ToHex
  , int32ToHex
  , int64ToHex
  , integerToHex
  , wordToHex
  , word8ToHex
  , word16ToHex
  , word32ToHex
  , word64ToHex
  , charToHex
  , stringToHex
  , byteStringToHex
  ) where

import Data.Bits (Bits, (.&.), complement, rotateR, shiftR)
import Data.Char (intToDigit, ord)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.ByteString as B
import qualified Data.Vector as V

charToHex :: Char -> String
charToHex = intToHex . ord

integerToHex :: Integer -> String
integerToHex = concatMap word8ToHex . bytes

intToHex :: Int -> String
intToHex = concatMap word8ToHex . bytes

int8ToHex :: Int8 -> String
int8ToHex = word8ToHex . fromIntegral

int16ToHex :: Int16 -> String
int16ToHex = concatMap word8ToHex . word16Bytes .fromIntegral

int32ToHex :: Int32 -> String
int32ToHex = concatMap word8ToHex . word32Bytes . fromIntegral

int64ToHex :: Int64 -> String
int64ToHex = concatMap word8ToHex . word64Bytes . fromIntegral

wordToHex :: Word -> String
wordToHex = concatMap word8ToHex . bytes

word8ToHex :: Word8 -> String
word8ToHex = V.unsafeIndex word8Strings . fromIntegral

word16ToHex :: Word16 -> String
word16ToHex = concatMap word8ToHex . word16Bytes

word32ToHex :: Word32 -> String
word32ToHex = concatMap word8ToHex . word32Bytes

word64ToHex :: Word64 -> String
word64ToHex = concatMap word8ToHex . word64Bytes

word16Bytes :: Word16 -> [Word8]
word16Bytes n =
    [ fromIntegral $ n `shiftR` 8
    , fromIntegral n
    ]

word32Bytes :: Word32 -> [Word8]
word32Bytes n =
    [ fromIntegral $ n `shiftR` 24
    , fromIntegral $ n `shiftR` 16
    , fromIntegral $ n `shiftR` 8
    , fromIntegral n
    ]

word64Bytes :: Word64 -> [Word8]
word64Bytes n =
    [ fromIntegral $ n `shiftR` 56
    , fromIntegral $ n `shiftR` 48
    , fromIntegral $ n `shiftR` 40
    , fromIntegral $ n `shiftR` 32
    , fromIntegral $ n `shiftR` 24
    , fromIntegral $ n `shiftR` 16
    , fromIntegral $ n `shiftR` 8
    , fromIntegral n
    ]

stringToHex :: String -> String
stringToHex = concatMap charToHex

byteStringToHex :: B.ByteString -> String
byteStringToHex = concatMap word8ToHex . B.unpack

--

bytes :: (Integral a, Bits a) => a -> [Word8]
bytes i = 
    f i []
  where
    f n a
      | n == 0 = a
      | otherwise = f ((n .&. mask) `rotateR` 8) (fromIntegral n .&. 0xff : a)
    mask = complement 0xff

word8Strings :: V.Vector String
word8Strings =
    V.generate 256 generate
  where
    generate i = [l,r]
      where
        l = intToDigit $ i `shiftR` 4
        r = intToDigit $ i .&. 0xf

