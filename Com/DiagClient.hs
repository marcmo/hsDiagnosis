module Com.DiagClient
    (
      module Com.DiagMessage,
      sendBytes,
      sendDiagMsg,
      sendData,
      sendDataTo,
      string2hex,
      Word8,
      DiagConfig(..)
    )    
where

import Com.DiagMessage
import Network(PortID(PortNumber),connectTo)
import System.IO
import Control.Concurrent(putMVar,MVar,forkIO,newEmptyMVar,takeMVar)
import Com.HSFZMessage
import Foreign(Ptr,Word8,free,mallocBytes)
import Foreign.C.String(peekCStringLen)
import Foreign.C.Types(CChar)
import Control.Monad.Reader(runReaderT,asks,liftIO,when,ReaderT)
import Control.OldException -- *** for base-4
import Text.Printf(printf)
import Util(string2hex)
import Prelude hiding (catch,log)

receiveBufSize = 4096
pollingMs = 100

data DiagConnection = DiagConnection { diagHandle :: Handle, verboseL :: Bool }
data DiagConfig = MkDiagConfig {
  host :: String,
  port :: Int,
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
    h <- connectTo (host c) (PortNumber $ fromIntegral (port c))
    hSetBuffering h NoBuffering
    return (DiagConnection h (verbose c))
  where
    notify
      | verbose c = bracket_ (printf "Connecting to %s ... " (host c) >> hFlush stdout) (putStrLn "done.")
      | otherwise = bracket_ (return ()) (return ())

run :: HSFZMessage -> Net (Maybe HSFZMessage)
run msg = do
    h <- asks diagHandle
    v <- asks verboseL
    b <- io newEmptyMVar
    io $ forkIO $ listenForResponse b h v
    io (pushOutMessage msg h v)
    io $ takeMVar b 

pushOutMessage :: HSFZMessage -> Handle -> Bool -> IO ()
pushOutMessage msg handle verbose = do
    when verbose $ printPayload "-->" msg
    hPutStr handle (msg2ByteString msg)
    hFlush handle -- Make sure that we send data immediately

listenForResponse ::  MVar (Maybe HSFZMessage) -> Handle -> Bool -> IO ()
listenForResponse m h verbose = do
    msg <- receiveResponse h verbose
    putMVar m msg
    return ()

receiveResponse :: Handle -> Bool -> IO (Maybe HSFZMessage)
receiveResponse h verbose = do
    buf <- mallocBytes receiveBufSize
    dataResp <- receiveDataMsg h buf verbose
    free buf
    if responsePending dataResp
      then print "...received response pending" >> receiveResponse h verbose 
      else return dataResp

receiveDataMsg ::  Handle -> Ptr CChar -> Bool -> IO (Maybe HSFZMessage)
receiveDataMsg h buf v = do
    msg <- receiveMsg h buf v
    when v $ maybe (return ()) (printPayload "<--") msg
    maybe (return Nothing)
      (\m->if isData m then log v "was data!" >> return msg else log v "was no data packet" >> receiveDataMsg h buf v) msg

responsePending ::  Maybe HSFZMessage -> Bool
responsePending = 
  maybe False (\m->
    let p = diagPayload (hsfz2diag m) in
      length p == 3 && p!!0 == 0x7f && p!!2 == 0x78)

receiveMsg ::  Handle -> Ptr CChar -> Bool -> IO (Maybe HSFZMessage)
receiveMsg h buf verbose = do
    dataAvailable <- waitForData h diagTimeout verbose
    if not dataAvailable then print "no message available..." >> return Nothing
      else do
        answereBytesRead <- hGetBufNonBlocking h buf receiveBufSize
        res2 <- peekCStringLen (buf,answereBytesRead)
        return $ bytes2msg res2

waitForData ::  Handle -> Int -> Bool -> IO (Bool)
waitForData h waitTime_ms verbose = do
  putStr "."
  when verbose $ printf "waitForData %d ms\n" waitTime_ms
  inputAvailable <- hWaitForInput h pollingMs
  if inputAvailable then return True 
    else if waitTime_ms > 0
          then waitForData h (waitTime_ms - pollingMs) verbose
          else return False

-- Convenience.
io :: IO a -> Net a
io = liftIO
log ::  (Show a) => Bool -> a -> IO ()
log v = when v . print
