module Com.HSFZMessage

where

import Data.Word(Word8)
import Data.List(intersperse)
import Util.Encoding
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary
import Debug.Trace

headerLen = 6

data ControlBit = AckBit | DataBit deriving (Show,Eq)
control2Int DataBit = 1 :: Word8
control2Int AckBit = 2 :: Word8
int2control 1 = DataBit
int2control 2 = AckBit
int2control x = error $ "int2control not defined for: " ++ show x 

data HSFZMessage = HSFZMessage {
  controllBit :: ControlBit,
  payloadLen :: Int,
  payload :: [Word8]
} deriving (Eq)

instance Show HSFZMessage where
  show (HSFZMessage _ _ xs) = showAsHexString xs
instance Binary HSFZMessage where
  put m = do
    putWord32be $ fromIntegral $ payloadLen m
    putWord8 0
    putWord8 $ control2Int $ controllBit m
    putByteString $ S.pack $ payload m
  get = do
    len <- getWord32be
    _ <- getWord8 -- not needed
    cbit <- getWord8
    payload <- getBytes (fromIntegral len)
    return $ HSFZMessage (int2control cbit) (fromIntegral len) (S.unpack payload)

dataMessage :: [Word8] -> HSFZMessage
dataMessage xs = HSFZMessage DataBit (length xs) xs

isAck :: HSFZMessage -> Bool
isAck m = controllBit m == AckBit

isData = not . isAck

msg2ByteString :: HSFZMessage -> S.ByteString
msg2ByteString m@(HSFZMessage bit len payload) =
  strict $ runPut $ put m

bytes2msg :: S.ByteString -> Maybe HSFZMessage
bytes2msg s = 
  if (S.length s < headerLen) || (S.length s /= headerLen + payloadLength)
    then Nothing
    else Just $ runGet get (strictToLazy s)
      where payloadLength = parseLength s

parseLength :: S.ByteString -> Int
parseLength s = fromIntegral len
  where (len,_,_) = runGetState getWord32be (strictToLazy s) 0

strictToLazy :: S.ByteString -> L.ByteString
strictToLazy = L.fromChunks . return

strict :: L.ByteString -> S.ByteString
strict  = S.concat . L.toChunks


