module Com.DiagMessage

where

import Data.Word(Word8)
import Util.Encoding
import Com.HSFZMessage


data DiagnosisMessage = DiagnosisMessage {
  diagSource :: Word8,
  diagTarget :: Word8,
  diagPayload :: [Word8]
} deriving (Eq)
instance Show DiagnosisMessage where
  show (DiagnosisMessage s t xs)  = '[':showAsHex s ++ "][" ++ showAsHex t ++ "]"
      ++ showAsHexString xs

diagMsg2bytes ::  DiagnosisMessage -> [Word8]
diagMsg2bytes m = diagSource m:diagTarget m:diagPayload m

matchPayload :: DiagnosisMessage -> [Word8] -> Bool
matchPayload (DiagnosisMessage _ _ p) exp =
  length p >= length exp && take (length exp) p == exp

diag2hsfz :: DiagnosisMessage -> ControlBit -> HSFZMessage
diag2hsfz dm cb = HSFZMessage cb (length payload) payload
  where payload = diagMsg2bytes dm

hsfz2diag ::  HSFZMessage -> DiagnosisMessage
hsfz2diag hm = DiagnosisMessage (diagBytes!!0) (diagBytes!!1) (drop 2 diagBytes)
  where diagBytes = payload hm

stream2diag ::  MessageStream -> [DiagnosisMessage]
stream2diag (MessageStream xs) = map hsfz2diag xs


