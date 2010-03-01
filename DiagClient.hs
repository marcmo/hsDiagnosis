module DiagClient
    (
      module DiagMessage,
      sendBytes,
      sendDiagMsg,
      sendData,
      sendDataTo,
      string2hex,
      Word8,
      DiagConfig(..)
    )    
where

import DiagMessage
import Network.Socket
import Network
import System.IO
import Control.Concurrent
import Control.Monad
import Maybe
import HSFZMessage
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Control.Monad.Reader
import Control.OldException -- *** for base-4
import Text.Printf
import Control.Applicative
import Util
import Prelude hiding (catch)

receiveBufSize = 4096
pollingMs = 100
verbosityLevel = True -- TODO: use configruation verbosity level

data DiagConnection = DiagConnection { diagHandle :: Handle }
data DiagConfig = MkDiagConfig {
  host :: String,
  port :: String,
  source :: String,
  target :: String,
  verbose :: Bool
} deriving (Show)

type Net = ReaderT DiagConnection IO
-- TODO: use ByteString
sendData ::  DiagConfig -> [Word8] -> IO (Maybe DiagnosisMessage)
sendData c = sendDiagMsg c . DiagnosisMessage (string2hex $ source c) (string2hex $ target c)

sendDataTo :: DiagConfig -> [Word8] -> Word8 -> Word8 -> IO (Maybe DiagnosisMessage)
sendDataTo c xs src target = (sendDiagMsg c . DiagnosisMessage src target) xs

sendMessage :: DiagConfig -> HSFZMessage -> IO (Maybe HSFZMessage)
sendMessage c msg = bracket (diagConnect c) disconnect loop
  where
    disconnect = hClose . diagHandle
    loop st    = catch (runReaderT (run msg) st) (\(IOException _) -> return Nothing)

sendBytes :: DiagConfig -> [Word8] -> IO (Maybe HSFZMessage)
sendBytes c = sendMessage c . dataMessage
sendDiagMsg :: DiagConfig -> DiagnosisMessage -> IO (Maybe DiagnosisMessage)
sendDiagMsg c dm = do
    let hsfzMsg = diag2hsfz dm DataBit
    hsfzResp <- sendMessage c hsfzMsg
    return $ maybe Nothing (Just . hsfz2diag) hsfzResp
 
diagConnect :: DiagConfig -> IO DiagConnection
diagConnect c = notify $ do
    -- addrinfos <- getAddrInfo Nothing (Just $ host c) (Just $ port c)
    -- let serveraddr = head addrinfos
    -- sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    -- setSocketOption sock KeepAlive 1
    -- connect sock (addrAddress serveraddr)
    -- h <- socketToHandle sock ReadWriteMode
    let p = read (port c) :: Int
    h <- connectTo (host c) (PortNumber $ fromIntegral p)
    hSetBuffering h NoBuffering
    return (DiagConnection h)
  where
    notify
      | verbose c = bracket_ (printf "Connecting to %s ... " (host c) >> hFlush stdout) (putStrLn "done.")
      | otherwise = bracket_ (return ()) (return ())

run :: HSFZMessage -> Net (Maybe HSFZMessage)
run msg = do
    h <- asks diagHandle
    b <- io newEmptyMVar
    io $ forkIO $ listenForResponse b h
    io (pushOutMessage msg h)
    io $ takeMVar b 

pushOutMessage :: HSFZMessage -> Handle -> IO ()
pushOutMessage msg handle = do
    when verbosityLevel $ printPayload "-->" msg
    hPutStr handle (msg2ByteString msg)
    hFlush handle -- Make sure that we send data immediately

listenForResponse ::  MVar (Maybe HSFZMessage) -> Handle -> IO ()
listenForResponse m h = do
    msg <- receiveResponse h
    putMVar m msg
    return ()

receiveResponse :: Handle -> IO (Maybe HSFZMessage)
receiveResponse h = do
    buf <- mallocBytes receiveBufSize
    dataResp <- receiveDataMsg h buf
    free buf
    if responsePending dataResp
      then print "...received response pending" >> receiveResponse h 
      else return dataResp

receiveDataMsg ::  Handle -> Ptr CChar -> IO (Maybe HSFZMessage)
receiveDataMsg h buf = do
    msg <- receiveMsg h buf
    when verbosityLevel $ maybe (return ()) (printPayload "<--") msg
    maybe (return Nothing)
      (\m->if isData m then print "was data!" >> return msg else print "was no data packet" >> receiveDataMsg h buf) msg

responsePending ::  Maybe HSFZMessage -> Bool
responsePending = 
  maybe False (\m->matchPayload (hsfz2diag m) [0x7F,0x19,0x78])

receiveMsg ::  Handle -> Ptr CChar -> IO (Maybe HSFZMessage)
receiveMsg h buf = do
    dataAvailable <- waitForData h diagTimeout
    if not dataAvailable then print "no message available..." >> return Nothing
      else do
        answereBytesRead <- hGetBufNonBlocking h buf receiveBufSize
        res2 <- peekCStringLen (buf,answereBytesRead)
        return $ bytes2msg res2

waitForData ::  Handle -> Int -> IO (Bool)
waitForData h waitTime_ms = do
  putStr "."
  -- printf "waitForData %d ms\n" waitTime_ms
  inputAvailable <- hWaitForInput h pollingMs
  if inputAvailable then return True 
    else if waitTime_ms > 0
          then waitForData h (waitTime_ms - pollingMs)
          else return False

-- Convenience.
io :: IO a -> Net a
io = liftIO
