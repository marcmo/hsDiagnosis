{-# LANGUAGE DeriveDataTypeable #-}
module Diag.Com.HSFZMessage

where

import Data.Word(Word8)
import Diag.Util.Encoding(showBinString,showAsHexString,showAsHex)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize
import Data.Typeable
import Data.Monoid
import Control.Exception

headerLen = 6

data ControlBit = AckBit | DataBit | GetVehicleIdentBit deriving (Show,Eq)
control2Int DataBit = 0x1 :: Word8
control2Int AckBit = 0x2 :: Word8
control2Int GetVehicleIdentBit = 0x11 :: Word8
int2control cbit
  | cbit == 0x1 = DataBit
  | cbit == 0x2 = AckBit
  | cbit == 0x11 = GetVehicleIdentBit
  | otherwise = error $ "int2control not defined for: " ++ showAsHex cbit

data HSFZMessage = HSFZMessage {
  controllBit :: ControlBit,
  payloadLen :: Int,
  payload :: [Word8]
} deriving (Eq)

newtype MessageStream = MessageStream { unstream :: [HSFZMessage] } deriving (Eq)
instance Monoid MessageStream where
  mempty = MessageStream []
  (mappend) (MessageStream a) (MessageStream b) = MessageStream (a ++ b)

instance Show HSFZMessage where
  show (HSFZMessage b len xs) = "<" ++ show b ++ "," ++ show len ++ "> " ++ showAsHexString xs
instance Show MessageStream where
  show (MessageStream []) = ""
  show (MessageStream (x:xs)) = show x ++ show (MessageStream xs)
instance Serialize HSFZMessage where
  put m = do
    putWord32be $ fromIntegral $ payloadLen m
    putWord8 0
    putWord8 $ control2Int $ controllBit m
    putByteString $ S.pack $ payload m
  get = do
    len <- getWord32be
    _ <- getWord8 -- not needed
    cbit <- getWord8
    p <- getBytes (fromIntegral len)
    return $ HSFZMessage (int2control cbit) (fromIntegral len) (S.unpack p)
instance Serialize MessageStream where
  put (MessageStream xs) = mapM_ put xs
  get = do
    m <- get :: Get HSFZMessage
    restLen <- remaining
    if restLen > 0
    	then do
    	  MessageStream xs <- get
    	  return $ MessageStream (m:xs)
    	else return $ MessageStream [m]

dataMessage :: [Word8] -> HSFZMessage
dataMessage xs = HSFZMessage DataBit (length xs) xs

isAck :: HSFZMessage -> Bool
isAck m = controllBit m == AckBit

isData = not . isAck

msg2ByteString :: HSFZMessage -> S.ByteString
msg2ByteString = runPut . put

data HsfzException = InvalidMessageException String
    deriving (Show, Typeable)

instance Exception HsfzException
deserialize2Hsfz :: S.ByteString -> Maybe HSFZMessage
deserialize2Hsfz s
  | invalid = (throw $ InvalidMessageException (showBinString s)) Nothing
  | otherwise = either (const Nothing) Just (runGet get s)
      where payloadLength = parseLength s
            invalid = (S.length s < headerLen) || (S.length s /= headerLen + payloadLength)

deserialize2HsfzStream :: S.ByteString -> MessageStream
deserialize2HsfzStream s = either
    (const (MessageStream []))
    id
    (runGet get s)
serializeFromStream :: MessageStream -> S.ByteString
serializeFromStream = runPut . put

parseLength :: S.ByteString -> Int
parseLength s = either (const 0) fromIntegral $ runGet getWord32be s

strictToLazy :: S.ByteString -> L.ByteString
strictToLazy = L.fromChunks . return

strict :: L.ByteString -> S.ByteString
strict  = S.concat . L.toChunks


