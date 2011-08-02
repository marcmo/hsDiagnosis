{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Com.DiagClient
    (
      module Com.DiagMessage,
      sendBytes,
      sendDiagMsg,
      sendData,
      sendDataAsync,
      sendDataTo,
      string2hex,
      Word8,
      DiagConfig(..)
    )    
where

import Com.DiagMessage
import Com.HSFZMessage
import Com.DiagBase
import Diag.DiagnosisCodes
import Network(PortID(PortNumber),connectTo)
import System.IO hiding (hPutStrLn,hPutStr)
import Data.ByteString.Char8 hiding (putStrLn,putStr)
import qualified Data.ByteString as S
import Control.Concurrent(putMVar,MVar,forkIO,newEmptyMVar,takeMVar,yield)
import Foreign(Ptr,Word8,free,mallocBytes)
import Foreign.C.String(peekCStringLen)
import Foreign.C.Types(CChar)
import Control.Monad.Reader
import Control.Exception
import Text.Printf(printf)
import Util.Encoding
import Debug.Trace
import Prelude hiding (catch,log)

sendData ::  DiagConfig -> [Word8] -> IO (Maybe DiagnosisMessage)
sendData c xs = do
  print $ "send to " ++ host c
  resp <- sendDiagMsg c False $ DiagnosisMessage (source c) (target c) xs
  print $ show resp
  when (isNegativeResponse resp) $ do
    let (Just (DiagnosisMessage _ _ (_:_:err:_))) = resp
    print err
    print $ "negative response: " ++ nameOfError err
  return resp

sendDataAsync ::  DiagConfig -> [Word8] -> IO ()
sendDataAsync c xs = do
  print $ "send async to " ++ host c
  sendDiagMsg c True $ DiagnosisMessage (source c) (target c) xs
  return ()

sendDataTo :: DiagConfig -> [Word8] -> Word8 -> Word8 -> IO (Maybe DiagnosisMessage)
sendDataTo c xs src target = (sendDiagMsg c False . DiagnosisMessage src target) xs

sendBytes :: DiagConfig -> [Word8] -> IO (Maybe HSFZMessage)
sendBytes c = sendMessage c False . dataMessage

sendDiagMsg :: DiagConfig -> Bool -> DiagnosisMessage -> IO (Maybe DiagnosisMessage)
sendDiagMsg c async dm = do
    let hsfzMsg = diag2hsfz dm DataBit
    hsfzResp <- sendMessage c async hsfzMsg
    return $ maybe Nothing (Just . hsfz2diag) hsfzResp

sendMessage :: DiagConfig -> Bool -> HSFZMessage -> IO (Maybe HSFZMessage)
sendMessage c async msg = bracket diagConnect disconnect loop
  where
    disconnect = hClose . diagHandle
    loop st    = catch (runReaderT (run async msg) st) (\(_ :: IOException) -> return Nothing)
    diagConnect = notify $ do
        h <- connectTo (host c) (PortNumber $ fromIntegral (port c))
        hSetBuffering h NoBuffering
        return (MkDiagConnection h (verbose c) (diagTimeout c))
    notify
      | verbose c = bracket_ (printf "Connecting to %s ... " (host c) >> hFlush stdout) (putStrLn "done.")
      | otherwise = bracket_ (return ()) (return ())

run :: Bool -> HSFZMessage -> Net (Maybe HSFZMessage)
run async msg = do
    case async of
      True -> pushOutMessage msg >> return Nothing
      False -> do
        m <- io newEmptyMVar
        ReaderT $ \r ->
          forkIO $ catch (runReaderT (listenForResponse m) r) (\(e :: HsfzException) -> print e >> putMVar m Nothing >> return ())
        -- io $ putStrLn "hickup"
        pushOutMessage msg
        io $ takeMVar m 
  where
    pushOutMessage :: HSFZMessage -> Net ()
    pushOutMessage msg = do
        h <- asks diagHandle
        log ("sending --> " ++ show msg)
        io $ hPutStr h (msg2ByteString msg)
        io $ hFlush h -- Make sure that we send data immediately

listenForResponse ::  MVar (Maybe HSFZMessage) -> Net ()
listenForResponse m =
   do msg <- receiveResponse
      io $ putMVar m msg
      return ()
  where

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
        maybe (return ()) (\m -> log $ "<-- " ++ show m) msg
        maybe (return Nothing)
          (\m->if isData m
            then (log "was data!") >> return msg
            else (log "was no data packet") >> receiveDataMsg buf) msg

    receiveMsg :: Ptr CChar -> Net (Maybe HSFZMessage)
    receiveMsg buf = do
        h <- asks diagHandle
        timeout <- asks connectionTimeout
        log ("wait for data with timeout:" ++ show timeout ++ " ms\n")
        dataAvailable <- waitForData timeout
        log ("\n")
        if not dataAvailable then (io $ print "no message available...") >> return Nothing
          else do
            answereBytesRead <- io $ hGetBufNonBlocking h buf receiveBufSize
            res2 <- io $ S.packCStringLen (buf,answereBytesRead)
            log $ "received over the wire: " ++ (showBinString res2)
            let resp = deserialize2Hsfz res2
            return resp

    waitForData ::  Int -> Net (Bool)
    waitForData waitTime_ms = do
      h <- asks diagHandle
      io $ S.putStr "."
      io $ yield
      -- log (".")
      inputAvailable <- io $ hWaitForInput h pollingMs
      if inputAvailable then return True 
        else if waitTime_ms > 0
              then waitForData (waitTime_ms - pollingMs)
              else return False

