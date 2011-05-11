{-# LANGUAGE ScopedTypeVariables #-}
module TestSender where

import Com.DiagMessage
import Com.HSFZMessage
import Diag.DiagnosisCodes
import Network(PortID(PortNumber),connectTo)
import System.IO
import Control.Concurrent(putMVar,MVar,forkIO,newEmptyMVar,takeMVar)
import Foreign(Ptr,Word8,free,mallocBytes)
import Foreign.C.String(peekCStringLen)
import Foreign.C.Types(CChar)
import Control.Monad.Reader
import Control.Exception
import Text.Printf(printf)
import Util.Encoding(string2hex)
import Debug.Trace
import Prelude hiding (catch,log)

receiveBufSize = 4096
pollingMs = 100

data DiagConnection = DiagConnection { diagHandle :: Handle, chatty :: Bool }
data DiagConfig = MkDiagConfig {
  host :: String,
  port :: Int,
  source :: Word8,
  target :: Word8,
  verbose :: Bool
} deriving (Show)

type Net = ReaderT DiagConnection IO

data LoggingInstruction = DISABLEALL
                        | ENABLEALL
                        | SETLEVEL
                        | DISPLAYSETTINGS
                           deriving (Eq, Ord, Show, Read, Enum)
toWord :: (Enum a) => a -> Word8
toWord = int2Word8 . fromEnum
int2Word8 x = fromIntegral x :: Word8
main = do
  let c = MkDiagConfig "10.40.39.19" 6801 0xf4 0x40 True
  sendData c [0xbf,0x12,0x04,toWord DISPLAYSETTINGS]

sendData ::  DiagConfig -> [Word8] -> IO ()
sendData c xs = do
  sendDiagMsg c $ DiagnosisMessage (source c) (target c) xs

sendDataTo :: DiagConfig -> [Word8] -> Word8 -> Word8 -> IO ()
sendDataTo c xs src target = (sendDiagMsg c . DiagnosisMessage src target) xs

sendMessage :: DiagConfig -> HSFZMessage -> IO ()
sendMessage c msg = bracket (diagConnect c) disconnect loop
  where
    disconnect = hClose . diagHandle
    loop st    = catch (runReaderT (run msg) st) (\(_ :: IOException) -> return ())

sendDiagMsg :: DiagConfig -> DiagnosisMessage -> IO ()
sendDiagMsg c dm = do
    let hsfzMsg = diag2hsfz dm DataBit
    sendMessage c hsfzMsg
 
diagConnect :: DiagConfig -> IO DiagConnection
diagConnect c = notify $ do
    h <- connectTo (host c) (PortNumber $ fromIntegral (port c))
    hSetBuffering h NoBuffering
    return (DiagConnection h (verbose c))
  where
    notify
      | verbose c = bracket_ (printf "Connecting to %s ... " (host c) >> hFlush stdout) (putStrLn "done.")
      | otherwise = bracket_ (return ()) (return ())

run :: HSFZMessage -> Net ()
run msg = do
    pushOutMessage msg

pushOutMessage :: HSFZMessage -> Net ()
pushOutMessage msg = do
    h <- asks diagHandle
    log ("--> " ++ show msg)
    io $ hPutStr h (msg2ByteString msg)
    io $ hFlush h -- Make sure that we send data immediately

-- Convenience.
io :: IO a -> Net a
io = liftIO
log ::  (Show a) => a -> Net ()
log s = do
    v <- asks chatty
    when v $ io $ print s
