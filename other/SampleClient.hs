import Com.DiagBase
import Com.DiagMessage
import Network(PortID(PortNumber),connectTo)
import Network.Socket
import Network.BSD(hostAddresses,getHostByName)
import System.IO
import Data.List(genericDrop)
import Control.Monad (liftM)
import Control.Applicative((<$>))
import Control.Exception
import Foreign(Ptr,Word8,free,mallocBytes)
import Foreign.C.Types(CChar)
import Foreign.C.String(peekCStringLen)
import Text.Printf(printf)
import Control.Concurrent(putMVar,MVar,forkIO,newEmptyMVar,takeMVar)

data SyslogHandle = 
    SyslogHandle {slSocket :: Socket,
                  slAddress :: SockAddr}

main = do
  h <- connectMe "127.0.0.1" diagPort
  m <- newEmptyMVar
  forkIO $ listenForResponse m h
  pushOutMessage h "echo\n"
  takeMVar m 
  hClose h

pushOutMessage h msg =
  syslog2 h msg >> hFlush h

listenForResponse ::  MVar (String) -> Handle -> IO ()
listenForResponse m h = do
   msg <- receiveResponse h
   putMVar m msg
   return ()

receiveResponse :: Handle -> IO (String)
receiveResponse h = do
    buf <- mallocBytes receiveBufSize
    dataResp <- receiveDataMsg h buf
    free buf
    return dataResp

receiveDataMsg ::  Handle -> Ptr CChar -> IO (String)
receiveDataMsg h buf = receiveMsg h buf 

receiveMsg :: Handle -> Ptr CChar -> IO (String)
receiveMsg h buf = do
    dataAvailable <- waitForData h 1000
    if not dataAvailable then (print "no message available...") >> return ""
      else do
        answereBytesRead <- hGetBufNonBlocking h buf receiveBufSize
        res2 <- peekCStringLen (buf,answereBytesRead)
        print $ "received over the wire: " ++ res2
        return $ res2

waitForData :: Handle -> Int -> IO (Bool)
waitForData h waitTime_ms = do
  putStr "."
  inputAvailable <- hWaitForInput h pollingMs
  if inputAvailable then return True 
    else if waitTime_ms > 0
          then waitForData h (waitTime_ms - pollingMs)
          else return False


openlogTcp :: HostName -> String -> IO Handle
openlogTcp hostname port = do 
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       setSocketOption sock KeepAlive 1
       connect sock (addrAddress serveraddr)
       h <- socketToHandle sock WriteMode
       hSetBuffering h (BlockBuffering Nothing)
       return $ h

syslog2 :: Handle -> String -> IO ()
syslog2 h msg = hPutStr h msg >> hFlush h

connectMe :: HostName -> Int -> IO Handle
connectMe hostname p = notify $ do
    h <- connectTo hostname (PortNumber $ fromIntegral p)
    hSetBuffering h NoBuffering
    return h
  where
    notify =  bracket_ (printf "Connecting to %s ... " hostname >> hFlush stdout) (putStrLn "done.")

