module Diag.Util.Encoding
where

import Data.Char
import Data.Word
import Data.Bits
import Numeric
import Data.List
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as S

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

string2hex ::  String -> Word8
string2hex = fst . head . readHex

string2hex16 ::  String -> Word16
string2hex16 = fst . head . readHex


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

showBinString ::  B.ByteString -> String
showBinString xs = showAsHexNumbers $ S.unpack xs

showAsHexNumbers :: [Word8] -> String
showAsHexNumbers xs = concat $ intersperse "," $ map (showAsHex . int2Word8) xs

nothingIf ::  Bool -> Maybe Int
nothingIf True = Nothing
nothingIf False = Just 0

convert :: String -> Char
convert = chr . fst . head . readHex

toWord :: (Enum a) => a -> Word8
toWord = int2Word8 . fromEnum

