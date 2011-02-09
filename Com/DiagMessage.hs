module Com.DiagMessage

where

import Text.Regex(splitRegex,mkRegex)
import Numeric(showHex,readHex)
import Data.Char(chr,ord)
import Data.Word(Word8)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Util.Encoding
import Debug.Trace
import Com.HSFZMessage
import Data.Bits
import Data.List(intersperse,intercalate)

diagTimeout = 5000 :: Int -- ms

data DiagnosisMessage = DiagnosisMessage {
  diagSource :: Word8,
  diagTarget :: Word8,
  diagPayload :: [Word8]
} deriving (Eq)
instance Show DiagnosisMessage where
  show (DiagnosisMessage s t xs)  = '[':(showAsHex s) ++ "][" ++ (showAsHex t) ++ "]"
      ++ (showAsHexString xs)

diagMsg2bytes ::  DiagnosisMessage -> [Word8]
diagMsg2bytes m = (diagSource m):(diagTarget m):(diagPayload m)

matchPayload :: DiagnosisMessage -> [Word8] -> Bool
matchPayload (DiagnosisMessage _ _ p) exp =
  length p >= length exp && (take (length exp) p) == exp

diag2hsfz :: DiagnosisMessage -> ControlBit -> HSFZMessage
diag2hsfz dm cb = HSFZMessage cb (length payload) payload
  where payload = diagMsg2bytes dm

hsfz2diag ::  HSFZMessage -> DiagnosisMessage
hsfz2diag hm = DiagnosisMessage (diagBytes!!0) (diagBytes!!1) (drop 2 diagBytes)
  where diagBytes = payload hm

showAsHexNumbers :: [Word8] -> String
showAsHexNumbers xs = concat $ intersperse "," $ map (showAsHex . int2Word8) xs

-- showBinString xs = let ys = map (ord) xs in
showBinString xs = let ys = S.unpack xs in
  showAsHexNumbers ys

nothingIf ::  Bool -> Maybe Int
nothingIf True = Nothing
nothingIf False = Just 0

convert :: String -> Char
convert = chr . fst . head . readHex

