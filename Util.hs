module Util

where

import Data.Char
import Data.Word
import Numeric
import Data.List

string2hex ::  String -> Word8
string2hex = fst . head . readHex

showAsHex ::  Word8 -> String
showAsHex = ((++) "0x") . (flip showHex "")

showAsHexString ::  [Word8] -> String
showAsHexString bs = '[':(intercalate "," $ map (showAsHex) bs) ++ "]"

