module Com.CanClient
where

import Data.Bits
import DiagnosticConfig
import Data.Word
import Com.DiagClient
import Util.Encoding

sampleCanMsg = CanMessage 100 10 0x555 [0xA,0xB,0xC]

data CanBus = Body_CAN
            | FA_CAN
            | K_CAN
            | D_CAN
            | ZSG_CAN
            | DEBUG_CAN
type CanID = Word16
data CanMessage = CanMessage {
  cycleTime :: Int,
  messageCount :: Int,
  canid :: CanID,
  messagePayload :: [Word8]
} deriving (Show,Eq)

msgTunnelConf = mkConf messageTunnel "10.40.39.53"

sendCanMsg :: CanBus -> CanMessage -> IO [DiagnosisMessage]
sendCanMsg can (CanMessage _cycleTime count canId payload) = do
  putStrLn $ showAsHexString messageTunnelmsg
  sendData msgTunnelConf messageTunnelmsg
    where messageTunnelmsg =  [0x31,0x01,0xF7,0x66]
                ++ can2Byte can:encodeInt _cycleTime 4
                ++ encodeInt count 4
                ++ encodeInt canId 2
                ++ [int2Word8 $ length payload]
                ++ payload

receiveCanMsg :: CanBus -> CanID -> Int -> Int -> IO [DiagnosisMessage]
receiveCanMsg can canId count timeout = do
  putStrLn $ showAsHexString messageTunnelmsg
  sendData msgTunnelConf messageTunnelmsg
    where messageTunnelmsg =  [0x31,0x01,0xF7,0x65]
                ++ can2Byte can:encodeInt count 2
                ++ encodeInt canId 2
                ++ encodeInt timeout 4


can2Byte :: CanBus -> Word8
can2Byte Body_CAN  = 1
can2Byte FA_CAN    = 1 `shiftL` 1
can2Byte K_CAN     = 1 `shiftL` 2
can2Byte D_CAN     = 1 `shiftL` 3
can2Byte ZSG_CAN   = 1 `shiftL` 4
can2Byte DEBUG_CAN = 1 `shiftL` 5

