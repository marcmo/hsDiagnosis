{-# LANGUAGE ScopedTypeVariables #-}
module Com.DiagClientEvent where

import Com.DiagMessage
import Com.HSFZMessage
import Diag.DiagnosisCodes
import Network(PortID(PortNumber),connectTo)
import System.IO
import System.CPUTime
import System.TimeIt
import Control.Concurrent(putMVar,MVar,forkIO,newEmptyMVar,takeMVar)
import Foreign(Ptr,Word8,free,mallocBytes)
import Foreign.C.String(peekCStringLen)
import Foreign.C.Types(CChar)
import Control.Monad.Reader
import Control.Exception
import Text.Printf(printf)
import System.Posix(sleep)
import Util.Encoding(string2hex)
import Control.Applicative((<$>))
import Debug.Trace
import Data.List(replicate)
import Prelude hiding (catch,log)
timeInSec :: IO Double
timeInSec = do
  t1 <- getCPUTime
  return $ (fromIntegral (t1) * 1e-6)

test = timeItT (print "hi")

pollingMs = 100
receiveBufSize = 4096
data DiagConnection = DiagConnection { diagHandle :: Handle, chatty :: Bool }
data DiagConfig = MkDiagConfig {
  host :: String,
  port :: Int,
  source :: Word8,
  target :: Word8,
  verbose :: Bool
} deriving (Show)
type Callback = Maybe DiagnosisMessage -> IO ()
type Net = ReaderT DiagConnection IO

sendWithCallback :: Int -> DiagConfig -> [Word8] -> Callback -> IO()
sendWithCallback listenTime c xs cb = sendMessageWithCallback listenTime c hsfzMsg cb
    where diagMsg = DiagnosisMessage (source c) (target c) xs
          hsfzMsg = diag2hsfz diagMsg DataBit

sendMessageWithCallback :: Int -> DiagConfig -> HSFZMessage -> Callback -> IO ()
sendMessageWithCallback listenTime c msg cb = bracket (diagConnect c) (disconnectWithTimeout 10) loop
  where
    disconnectWithTimeout t con =  do
          sleep t
          print "disconnecting....with timeout" >>(hClose . diagHandle) con
    loop st    = catch (runReaderT (run listenTime c cb msg) st) (\(err :: IOException) -> print err)


main :: IO ()
main = do
  -- sendWithCallback 10 c [0x22,0x20,0x00] cb
  sendWithCallback 5 c [ 0xbf,0x10,0x1,0x1 ] cb
  sendWithCallback 5 c [0xBF,0xFF,0x77,0x03,0x00,0xC8] cb
  sendWithCallback 10 c [0x31,0x01,0xF7,0x65,0x10,0xFF,0xFF,0x07,0x99,0x00,0x00,0x10,0x00] cb
  sendWithCallback 5 c [0xBF,0xFF,0x77,0xFF] cb
  sendWithCallback 5 c [0x31,0x02,0xF7,0x65] cb
    where c = MkDiagConfig "10.40.39.19" 6801 0xf4 0x40 True
          cb m = print $ "received s.th.:" ++ show m

diagConnect :: DiagConfig -> IO DiagConnection
diagConnect c = notify $ do
    h <- connectTo (host c) (PortNumber $ fromIntegral (port c))
    hSetBuffering h NoBuffering
    return (DiagConnection h (verbose c))
  where
    notify
      | verbose c = bracket_ (printf "Connecting to %s ... " (host c) >> hFlush stdout) (putStrLn "done.")
      | otherwise = bracket_ (return ()) (return ())

run :: Int -> DiagConfig -> Callback -> HSFZMessage -> Net ()
run listenTime c cb msg = do 
    ReaderT $ \r -> forkIO $ runReaderT (asks diagHandle >>= listen listenTime c cb) r
    pushOutMessage msg

pushOutMessage :: HSFZMessage -> Net ()
pushOutMessage msg = do
    h <- asks diagHandle
    log ("--> " ++ show msg)
    io $ hPutStr h (msg2ByteString msg)
    io $ hFlush h -- Make sure that we send data immediately

listen :: Int -> DiagConfig -> Callback -> Handle -> Net ()
listen listenTime c cb h = do -- forever $ do
    startTime <- io $ getCPUTime
    let repeatTimes = round $ (fromIntegral listenTime*1000)/(fromIntegral diagTimeout)
    io $ print ("repeating n times: " ++ show repeatTimes)
    sequence_ $ replicate repeatTimes (receiveResponse cb)
  where
    forever a = a >> forever a
 
receiveResponse :: Callback -> Net ()
receiveResponse cb = do
    io $ print "calling receiveResponse .............................."
    buf <- io $ mallocBytes receiveBufSize
    dataResp <- receiveDataMsg buf
    io $ free buf
    if responsePending dataResp
      then (log $ "...received response pending") >> receiveResponse cb
      else io $ cb $ hsfz2diag <$> dataResp

receiveDataMsg ::  Ptr CChar -> Net (Maybe HSFZMessage)
receiveDataMsg buf = do
    msg <- receiveMsg buf 
    log $ "was " ++ show msg
    maybe (return ()) (\m -> log $ "<-- " ++ show m) msg
    maybe (return Nothing)
      (\m->if isData m then (log "was data!") >> return msg else (log "was no data packet") >> receiveDataMsg buf) msg

receiveMsg ::  Ptr CChar -> Net (Maybe HSFZMessage)
receiveMsg buf = do
    h <- asks diagHandle
    dataAvailable <- io $ waitForData diagTimeout h
    if not dataAvailable then (io $ print "no message available...") >> return Nothing
      else do
        answereBytesRead <- io $ hGetBufNonBlocking h buf receiveBufSize
        res2 <- io $ peekCStringLen (buf,answereBytesRead)
        log $ "received over the wire: " ++ (showBinString res2)
        return $ bytes2msg res2

waitForData ::  Int -> Handle -> IO (Bool)
waitForData waitTime_ms h = do
  putStr "."
  inputAvailable <- hWaitForInput h pollingMs
  if inputAvailable then return True 
    else if waitTime_ms > 0
          then waitForData (waitTime_ms - pollingMs) h
          else return False

io :: IO a -> Net a
io = liftIO

log ::  (Show a) => a -> Net ()
log s = do
    v <- asks chatty
    when v $ io $ print s

responsePending ::  Maybe HSFZMessage -> Bool
responsePending = 
  maybe False (\m->
    let p = diagPayload (hsfz2diag m) in
      length p == 3 && p!!0 == 0x7f && p!!2 == 0x78)
