{-# LANGUAGE ScopedTypeVariables #-}
module Com.DiagClientCallback where

import Com.DiagMessage
import Com.HSFZMessage
import Com.DiagBase
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
import Maybe(isJust)
import Control.Applicative((<$>))
import Debug.Trace
import Prelude hiding (catch,log)

main = sendData c callback []
    where callback resp = when (isNegativeResponse resp) $ do
                  let (Just (DiagnosisMessage _ _ (_:_:err:_))) = resp
                  print err
                  print $ "negative response: " ++ nameOfError err
          -- c = MkDiagConfig "10.40.39.19" 6801 0xf4 0x40 True
          c = MkDiagConfig "localhost" 6801 0xf4 0x40 True
sendData ::  DiagConfig -> Callback -> [Word8] -> IO ()
sendData c cb xs = do
  sendDiagMsg c cb $ DiagnosisMessage (source c) (target c) xs

sendDataTo :: DiagConfig -> Callback -> [Word8] -> Word8 -> Word8 -> IO ()
sendDataTo c cb xs src target = (sendDiagMsg c cb . DiagnosisMessage src target) xs

sendDiagMsg :: DiagConfig -> Callback -> DiagnosisMessage -> IO ()
sendDiagMsg c callback dm = do
    let hsfzMsg = diag2hsfz dm DataBit
    sendMessage c callback hsfzMsg

sendMessage :: DiagConfig -> Callback -> HSFZMessage -> IO ()
sendMessage c callback msg = bracket (connect c) disconnect loop
  where
    disconnect = hClose . diagHandle
    loop st    = catch (runReaderT (run msg callback) st) (\(_ :: IOException) -> return ())
 
connect :: DiagConfig -> IO DiagConnection
connect c = notify $ do
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

run :: HSFZMessage -> Callback-> Net ()
run msg cb = do
    m <- io newEmptyMVar
    listenInThread m
      where listenInThread m = do
                ReaderT $ \r ->
                    forkIO $ runReaderT (listenForResponse m) r
                pushOutMessage msg
                resp <- io $ takeMVar m 
                io $ cb (hsfz2diag <$> resp)
                listenInThread m

pushOutMessage :: HSFZMessage -> Net ()
pushOutMessage msg = do
    h <- asks diagHandle
    log ("--> " ++ show msg)
    io $ hPutStr h (msg2ByteString msg)
    io $ hFlush h -- Make sure that we send data immediately

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

