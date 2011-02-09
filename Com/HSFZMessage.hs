module Com.HSFZMessage

where

import Data.Word(Word8)
import Data.List(intersperse)
import Util.Encoding
import Debug.Trace

data ControlBit = AckBit | DataBit deriving (Show,Eq)
control2Int DataBit = 1 :: Word8
control2Int AckBit = 2 :: Word8
int2control 1 = DataBit
int2control 2 = AckBit
int2control x = (trace $ "int2control not defined for: " ++ show x) undefined

data HSFZMessage = HSFZMessage {
  controllBit :: ControlBit,
  payloadLen :: Int,
  payload :: [Word8]
}

instance Show HSFZMessage where
  show (HSFZMessage _ _ xs) = showAsHexString xs

dataMessage :: [Word8] -> HSFZMessage
dataMessage xs = HSFZMessage DataBit (length xs) xs

isAck :: HSFZMessage -> Bool
isAck m = controllBit m == AckBit

isData = not . isAck

