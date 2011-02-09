module Util.Encoding

where

import Data.Char
import Data.Word
import Data.Bits
import Numeric
import Data.List

int2Word8 x = fromIntegral x :: Word8
word8ToInt x = fromIntegral x :: Int

encodeInt :: (Integral a, Bits a) => a -> Int -> [Word8]
encodeInt n width = 
            [int2Word8 $ 0xFF .&. (n `shiftR` s) | s <- reverse $ take width [0,8..]]

encodeLength :: Int -> [Word8]
encodeLength len =
            [0xFF .&. int2Word8 (len `shiftR` 24)
            ,0xFF .&. int2Word8 (len `shiftR` 16)
            ,0xFF .&. int2Word8 (len `shiftR` 8)
            ,0xFF .&. int2Word8 (len `shiftR` 0)]

-- decodeLength :: [Word8] -> Word32
-- decodeLength xs 
--   | length xs < 4 = error "insufficient length for decoding"
--   | otherwise =  (xs!!3) .|. 
--                 ((xs!!2) `shiftL` 8)  .|. 
--                 ((xs!!1) `shiftL` 16) .|. 
--                 ((xs!!0) `shiftL` 24)  

string2hex ::  String -> Word8
string2hex = fst . head . readHex

showAsHex ::  Word8 -> String
showAsHex = ((++) "0x") . (flip showHex "")

showAsHexOneString ::  [Word8] -> String
showAsHexOneString bs = "0x" ++ (concatMap showMin2Chars bs)

showMin2Chars :: Word8 -> String
showMin2Chars n =
  let x = flip showHex "" n 
      len = length x in 
    replicate (2 - len) '0' ++ x

showAsHexString ::  [Word8] -> String
showAsHexString bs = '[':(intercalate "," $ map (showAsHex) bs) ++ "]"

showAsBin :: Word8 -> String
showAsBin = flip showBin ""
  where showBin = showIntAtBase 2 intToDigit
