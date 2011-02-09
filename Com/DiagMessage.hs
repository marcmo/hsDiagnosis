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
import Data.Binary.Get
import Data.List(intersperse,intercalate)

headerLen = 6
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

msg2ByteString :: HSFZMessage -> S.ByteString
msg2ByteString (HSFZMessage bit len payload) =
  S.pack ((encodeLength len) ++ [0,control2Int bit] ++ payload)

-- msg2ints :: HSFZMessage -> String
-- msg2ints (HSFZMessage bit len payload) = 
--   showAsHexNumbers $ encodeLength len ++ [0,control2Int bit] ++ (map word8ToInt payload)

showAsHexNumbers :: [Word8] -> String
showAsHexNumbers xs = concat $ intersperse "," $ map (showAsHex . int2Word8) xs

-- showBinString xs = let ys = map (ord) xs in
showBinString xs = let ys = S.unpack xs in
  showAsHexNumbers ys

strict :: BL.ByteString -> S.ByteString
strict  = B.concat . LB.toChunks
strictToLazy :: S.ByteString -> BL.ByteString
strictToLazy = BL.fromChunks . return

bytes2msg :: S.ByteString -> Maybe HSFZMessage
bytes2msg s = 
  if (S.length s < headerLen) || (S.length s /= headerLen + payloadLength)
    then Nothing
    else return $ HSFZMessage b payloadLength (S.unpack $ S.drop headerLen s)
    -- else return $ HSFZMessage b payloadLength (map (int2Word8 . ord) $ S.drop headerLen s)
      where payloadLength = parseLength s
            b = (int2control . head . S.unpack . S.drop 5) s
            -- b = (int2control . ord . head . drop 5) s

nothingIf ::  Bool -> Maybe Int
nothingIf True = Nothing
nothingIf False = Just 0

parseLength :: S.ByteString -> Int
parseLength s = let (e,_,_) = runGetState getWord32be (strictToLazy s) 0
    in fromIntegral e
-- parseLength :: S.ByteString -> Int
-- parseLength bs = (ord $ s!!3) .|. 
--   ((ord $ s!!2) `shiftL` 8)  .|. 
--   ((ord $ s!!1) `shiftL` 16) .|. 
--   ((ord $ s!!0) `shiftL` 24)  
--     where s = S.unpack bs

convert :: String -> Char
convert = chr . fst . head . readHex

