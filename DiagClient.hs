module DiagClient
    (
      module DiagMessage,
      sendBytes,
      sendDiagMsg,
      sendData,
      string2hex,
      Word8
    )    
where

import DiagMessage
import Network.Socket
import System.IO
import DiagnosticConfig
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
 
type Net = ReaderT DiagConnection IO
-- TODO: use ByteString
sendData ::  [Word8] -> IO (Maybe DiagnosisMessage)
sendData = sendDiagMsg . DiagnosisMessage (string2hex source) (string2hex target)

sendDataTo :: [Word8] -> Word8 -> Word8 -> IO (Maybe DiagnosisMessage)
sendDataTo xs src target = (sendDiagMsg . DiagnosisMessage src target) xs

sendMessage :: HSFZMessage -> IO (Maybe HSFZMessage)
sendMessage msg = bracket (diagConnect) disconnect loop
  where
    disconnect = hClose . diagHandle
    loop st    = catch (runReaderT (run msg) st) (\(IOException _) -> return (Nothing))

sendBytes :: [Word8] -> IO (Maybe HSFZMessage)
sendBytes = sendMessage . dataMessage
sendDiagMsg :: DiagnosisMessage -> IO (Maybe DiagnosisMessage)
sendDiagMsg dm = do
    let hsfzMsg = diag2hsfz dm DataBit
    hsfzResp <- sendMessage hsfzMsg
    return $ maybe Nothing (Just . hsfz2diag) hsfzResp
 
diagConnect :: IO DiagConnection
diagConnect = notify $ do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering
    return (DiagConnection h)
  where
    notify = bracket_ (return ()) (return ())
        -- (printf "Connecting to %s ... " host >> hFlush stdout)
        -- (putStrLn "done.")

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
