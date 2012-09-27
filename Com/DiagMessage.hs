module Com.DiagMessage

where

import Data.Word(Word8)
import Util.Encoding
import Com.HSFZMessage

data DiagnosisMessage = DiagnosisMessage {
  diagSource  :: Word8,
  diagTarget  :: Word8,
  diagPayload :: [Word8]

} deriving (Eq)
instance Show DiagnosisMessage where
  show (DiagnosisMessage s t xs)  = '[':showAsHex s ++ "][" ++ showAsHex t ++ "]"
      ++ showAsHexString xs

diagMsg2bytes ::  DiagnosisMessage -> [Word8]
diagMsg2bytes m = diagSource m:diagTarget m:diagPayload m

matchPayload :: DiagnosisMessage -> [Word8] -> Bool
matchPayload (DiagnosisMessage _ _ p) e =
  length p >= length e && take (length e) p == e

diag2hsfz :: DiagnosisMessage -> ControlBit -> HSFZMessage
diag2hsfz dm cb = HSFZMessage cb (length p) p
  where p = diagMsg2bytes dm

hsfz2diag ::  HSFZMessage -> DiagnosisMessage
hsfz2diag hm = DiagnosisMessage (head diagBytes) (diagBytes!!1) (drop 2 diagBytes)
  where diagBytes = payload hm

stream2diag ::  MessageStream -> [DiagnosisMessage]
stream2diag (MessageStream xs) = map hsfz2diag xs


