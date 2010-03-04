module Com.HSFZMessage

where

import Data.Word(Word8)
import Data.List(intersperse)
import Util

data ControlBit = AckBit | DataBit deriving (Show,Eq)
control2Int DataBit = 1
control2Int AckBit = 2
int2control 1 = DataBit
int2control 2 = AckBit

data HSFZMessage = HSFZMessage {
  controllBit :: ControlBit,
  payloadLen :: Int,
  payload :: [Word8]
} deriving (Show)

dataMessage :: [Word8] -> HSFZMessage
dataMessage xs = HSFZMessage DataBit (length xs) xs

printPayload ::  String -> HSFZMessage -> IO ()
printPayload s msg = print $ s ++ showAsHexString (payload msg)

isAck :: HSFZMessage -> Bool
isAck m = controllBit m == AckBit

isData = not . isAck

