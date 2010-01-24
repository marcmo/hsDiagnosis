module DiagClient
    (
      module DiagMessage,
      sendBytes,
      sendDiagMsg,
      sendData,
      string2hex,
      Word8,
      DiagConfig(..)
    )    
where

import DiagMessage
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Monad
import HSFZMessage
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Control.Monad.Reader
import Control.OldException -- *** for base-4
import Text.Printf
import Control.Concurrent
import Util
import Prelude hiding (catch)

receiveBufSize = 4096
pollingMs = 100

data DiagConnection = DiagConnection { diagHandle :: Handle }
data DiagConfig = MkDiagConfig {
  host :: String,
  port :: String,
  source :: String,
  target :: String
} deriving (Show)
 
type Net = ReaderT DiagConnection IO
-- TODO: use ByteString
sendData ::  DiagConfig -> [Word8] -> IO (Maybe DiagnosisMessage)
sendData c = (sendDiagMsg c) . DiagnosisMessage (string2hex $ source c) (string2hex $ target c)

sendDataTo :: DiagConfig -> [Word8] -> Word8 -> Word8 -> IO (Maybe DiagnosisMessage)
sendDataTo c xs src target = (sendDiagMsg c . DiagnosisMessage src target) xs

sendMessage :: DiagConfig -> HSFZMessage -> IO (Maybe HSFZMessage)
sendMessage c msg = bracket (diagConnect c) disconnect loop
  where
    disconnect = hClose . diagHandle
    loop st    = catch (runReaderT (run msg) st) (\(IOException _) -> return (Nothing))

sendBytes :: DiagConfig -> [Word8] -> IO (Maybe HSFZMessage)
sendBytes c = (sendMessage c) . dataMessage
sendDiagMsg :: DiagConfig -> DiagnosisMessage -> IO (Maybe DiagnosisMessage)
sendDiagMsg c dm = do
    let hsfzMsg = diag2hsfz dm DataBit
    hsfzResp <- sendMessage c hsfzMsg
    return $ maybe Nothing (Just . hsfz2diag) hsfzResp
 
diagConnect :: DiagConfig -> IO DiagConnection
diagConnect c = notify $ do
    addrinfos <- getAddrInfo Nothing (Just $ host c) (Just $ port c)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering
    return (DiagConnection h)
  where
    notify = bracket_ -- (return ()) (return ())
        (printf "Connecting to %s ... " (host c) >> hFlush stdout)
        (putStrLn "done.")

-- We're in the Net monad now, so we've connected successfully
-- connected to a socket
run :: HSFZMessage -> Net (Maybe HSFZMessage)
run msg = do
    h <- asks diagHandle
    b <- io newEmptyMVar
    io $ forkIO $ listenForResponse b h
    io (pushOutMessage msg h)
    io $ takeMVar b 

pushOutMessage :: HSFZMessage -> Handle -> IO ()
pushOutMessage msg handle = do
    printPayload "-->" msg
    hPutStr handle (msg2ByteString msg)
    hFlush handle -- Make sure that we send data immediately

listenForResponse ::  MVar (Maybe HSFZMessage) -> Handle -> IO ()
listenForResponse m h = do
    buf <- mallocBytes receiveBufSize
    receiveMsg h buf
    msg <- receiveMsg h buf
    maybe (return ()) (printPayload "<--") msg
    free buf
    putMVar m msg
    return ()

receiveMsg ::  Handle -> Ptr CChar -> IO (Maybe HSFZMessage)
receiveMsg h buf = do
    waitForData h diagTimeout
    answereBytesRead <- hGetBufNonBlocking h buf receiveBufSize
    res2 <- peekCStringLen (buf,answereBytesRead)
    return $ bytes2msg res2

waitForData ::  Handle -> Int -> IO ()
waitForData h waitTime_ms = do
  inputAvailable <- hWaitForInput h pollingMs
  unless inputAvailable $ waitForData h (waitTime_ms - pollingMs)

-- Convenience.
io :: IO a -> Net a
io = liftIO
