{-# LANGUAGE ScopedTypeVariables #-}
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
-- TODO: use ByteString
sendData ::  DiagConfig -> [Word8] -> IO (Maybe DiagnosisMessage)
sendData c xs = do
  -- print $ "send to " ++ host c
  resp <- sendDiagMsg c $ DiagnosisMessage (source c) (target c) xs
  -- print $ show resp
  when (isNegativeResponse resp) $ do
    let (Just (DiagnosisMessage _ _ (_:_:err:_))) = resp
    print err
    print $ "negative response: " ++ nameOfError err
  return resp

isNegativeResponse Nothing = False
isNegativeResponse (Just (DiagnosisMessage _ _ (x:xs))) =
  x == negative_response_identifier

sendDataTo :: DiagConfig -> [Word8] -> Word8 -> Word8 -> IO (Maybe DiagnosisMessage)
sendDataTo c xs src target = (sendDiagMsg c . DiagnosisMessage src target) xs

sendMessage :: DiagConfig -> HSFZMessage -> IO (Maybe HSFZMessage)
sendMessage c msg = bracket (diagConnect c) disconnect loop
  where
    disconnect = hClose . diagHandle
    loop st    = catch (runReaderT (run msg) st) (\(_ :: IOException) -> return Nothing)

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
    b <- io newEmptyMVar
    ReaderT $ \r ->
      forkIO $ runReaderT (listenForResponse b) r
    pushOutMessage msg
    io $ takeMVar b 

pushOutMessage :: HSFZMessage -> Net ()
pushOutMessage msg = do
    h <- asks diagHandle
    log ("--> " ++ show msg)
    io $ hPutStr h (msg2ByteString msg)
    io $ hFlush h -- Make sure that we send data immediately

liftReader a = ReaderT (return . runReader a)

listenForResponse ::  MVar (Maybe HSFZMessage) -> Net ()
listenForResponse m = do
   msg <- receiveResponse
   io $ putMVar m msg
   return ()

receiveResponse :: Net (Maybe HSFZMessage)
receiveResponse = do
    buf <- io $ mallocBytes receiveBufSize
    dataResp <- receiveDataMsg buf
    io $ free buf
    if responsePending dataResp
      then (log $ "...received response pending") >> receiveResponse
      else return dataResp

receiveDataMsg ::  Ptr CChar -> Net (Maybe HSFZMessage)
receiveDataMsg buf = do
    msg <- receiveMsg buf 
    log $ "was " ++ show msg
    maybe (return ()) (\m -> log $ "<-- " ++ show m) msg
    maybe (return Nothing)
      (\m->if isData m then (log "was data!") >> return msg else (log "was no data packet") >> receiveDataMsg buf) msg

responsePending ::  Maybe HSFZMessage -> Bool
responsePending = 
  maybe False (\m->
    let p = diagPayload (hsfz2diag m) in
      length p == 3 && p!!0 == 0x7f && p!!2 == 0x78)

receiveMsg ::  Ptr CChar -> Net (Maybe HSFZMessage)
receiveMsg buf = do
    h <- asks diagHandle
    dataAvailable <- waitForData diagTimeout
    if not dataAvailable then (io $ print "no message available...") >> return Nothing
      else do
        answereBytesRead <- io $ hGetBufNonBlocking h buf receiveBufSize
        res2 <- io $ peekCStringLen (buf,answereBytesRead)
        log $ "received over the wire: " ++ (showBinString res2)
        return $ bytes2msg res2

waitForData ::  Int -> Net (Bool)
waitForData waitTime_ms = do
  h <- asks diagHandle
  io $ putStr "."
  log ("waitForData " ++ show waitTime_ms ++ " ms\n")
  inputAvailable <- io $ hWaitForInput h pollingMs
  if inputAvailable then return True 
    else if waitTime_ms > 0
          then waitForData (waitTime_ms - pollingMs)
          else return False

-- Convenience.
io :: IO a -> Net a
io = liftIO
log ::  (Show a) => a -> Net ()
log s = do
    v <- asks chatty
    when v $ io $ print s
