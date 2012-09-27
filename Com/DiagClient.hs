{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Com.DiagClient
    (
      module Com.DiagMessage,
      sendBytes,
      sendDiagMsg,
      sendData,
      sendMessage,
      sendDataAsync,
      sendDataTo,
      string2hex,
      Word8,
      DiagConfig(..)
    )    
where

import Com.DiagMessage
import Data.Monoid
import Com.HSFZMessage
import Com.DiagBase
import Network(PortID(PortNumber),connectTo)
import System.IO hiding (hPutStrLn,hPutStr)
import Data.ByteString.Char8 hiding (putStrLn,putStr,length,head,tail,filter)
import qualified Data.ByteString as S
import Control.Concurrent(putMVar,MVar,forkIO,newEmptyMVar,takeMVar,yield)
import Foreign(Ptr,Word8,free,mallocBytes)
import Foreign.C.Types(CChar)
import Control.Monad.Reader
import Control.Exception
import Text.Printf(printf)
import Util.Encoding
import Prelude hiding (catch,log)

sendData ::  DiagConfig -> [Word8] -> IO [DiagnosisMessage]
sendData c xs = do
  when (verbose c) (print $ "send to " ++ host c)
  resp <- sendDiagMsg c False $ DiagnosisMessage (source c) (target c) xs
  when (verbose c) (print $ show resp)
  printNegativeResponses resp
  return resp


sendDataAsync ::  DiagConfig -> [Word8] -> IO ()
sendDataAsync c xs = do
  print $ "send async to " ++ host c
  sendDiagMsg c True $ DiagnosisMessage (source c) (target c) xs
  return ()

sendDataTo :: DiagConfig -> [Word8] -> Word8 -> Word8 -> IO [DiagnosisMessage]
sendDataTo c xs src trgt = (sendDiagMsg c False . DiagnosisMessage src trgt) xs

sendBytes :: DiagConfig -> [Word8] -> IO MessageStream
sendBytes c = sendMessage c False . dataMessage

sendDiagMsg :: DiagConfig -> Bool -> DiagnosisMessage -> IO [DiagnosisMessage]
sendDiagMsg c async dm = do
    let hsfzMsg = diag2hsfz dm DataBit
    hsfzResp <- sendMessage c async hsfzMsg
    return $ stream2diag hsfzResp

sendMessage :: DiagConfig -> Bool -> HSFZMessage -> IO MessageStream
sendMessage c async msg = bracket diagConnect disconnect loop
  where
    disconnect = hClose . diagHandle
    loop st    = catch (runReaderT (run async msg) st) (\(_ :: IOException) -> return mempty)
    diagConnect = notify $ do
        h <- connectTo (host c) (PortNumber $ fromIntegral (port c))
        hSetBuffering h NoBuffering
        return (MkDiagConnection h (verbose c) (diagTimeout c))
    notify
      | verbose c = bracket_ (printf "Connecting to %s ... " (host c) >> hFlush stdout) (putStrLn "done.")
      | otherwise = bracket_ (return ()) (return ())

run :: Bool -> HSFZMessage -> Net MessageStream
run async msg =
    if async then pushOutMessage msg >> return mempty
      else do
        m <- io newEmptyMVar
        ReaderT $ \r ->
          forkIO $ catch
                    (runReaderT (listenForResponse m) r)
                    (\(e :: HsfzException) -> (void (print e >> putMVar m mempty)))
        -- io $ putStrLn "hickup"
        pushOutMessage msg
        io $ takeMVar m 
  where
    pushOutMessage :: HSFZMessage -> Net ()
    pushOutMessage m = do
        h <- asks diagHandle
        log ("sending --> " ++ show m)
        io $ hPutStr h (msg2ByteString m)
        io $ hFlush h -- Make sure that we send data immediately

listenForResponse ::  MVar MessageStream -> Net ()
listenForResponse m =
   do msg <- receiveResponse
      io $ putMVar m msg
      return ()
  where

    receiveResponse :: Net MessageStream
    receiveResponse = do
        buf <- io $ mallocBytes receiveBufSize
        msgStream@(MessageStream xs) <- receiveDataMsg buf
        io $ free buf
        if length xs == 1 && responsePending (head xs)
          then log "...received response pending" >> receiveResponse
          else return msgStream

    receiveDataMsg ::  Ptr CChar -> Net MessageStream
    receiveDataMsg buf = do
        stream@(MessageStream xs) <- receiveMsg buf 
        if mempty == stream
          then log ("<-- " ++ show stream) >> return stream
          else
            if length (filter isData xs) > 0
              then log "was data!" >> return stream
              else log "was no data packet" >> receiveDataMsg buf

    receiveMsg :: Ptr CChar -> Net MessageStream
    receiveMsg buf = do
        h <- asks diagHandle
        timeout <- asks connectionTimeout
        log ("wait for data with timeout:" ++ show timeout ++ " ms\n")
        dataAvailable <- waitForData timeout
        log "\n"
        if not dataAvailable then io (print "no message available...") >> return mempty
          else do
            answereBytesRead <- io $ hGetBufNonBlocking h buf receiveBufSize
            res2 <- io $ S.packCStringLen (buf,answereBytesRead)
            log $ "received over the wire: " ++ showBinString res2
            return $ deserialize2HsfzStream res2

    waitForData ::  Int -> Net Bool
    waitForData waitTime_ms = do
      h <- asks diagHandle
      io $ S.putStr "."
      io yield
      -- log (".")
      inputAvailable <- io $ hWaitForInput h pollingMs
      if inputAvailable then return True 
        else if waitTime_ms > 0
              then waitForData (waitTime_ms - pollingMs)
              else return False

