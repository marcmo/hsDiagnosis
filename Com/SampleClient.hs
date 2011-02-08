import Data.Bits
import Com.DiagBase
import Network(PortID(PortNumber),connectTo)
import Network.Socket
import Network.BSD
import System.IO
import Data.List
import Control.Monad (liftM)

data SyslogHandle = 
    SyslogHandle {slSocket :: Socket,
                  slAddress :: SockAddr}

main = do
  s <- openlog "127.0.0.1" (show diagPort)
  syslog s "test"
  closelog s
  h <- connectTo2 "127.0.0.1" diagPort
  syslog2 h "test2"
  closelog2 h

connectTo2 :: String -> Int -> IO Handle
connectTo2 host port_ = do
      let port = toEnum port_
      sock <- socket AF_INET Stream 0
      addrs <- liftM hostAddresses $ getHostByName host
      if null addrs then error $ "no such host : " ++ host else return ()
      connect sock $ SockAddrInet port (head addrs)
      handle <- socketToHandle sock ReadWriteMode
      return handle

openlog :: HostName -> String -> IO SyslogHandle 
openlog hostname port =
    do addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
       return $ SyslogHandle sock (addrAddress serveraddr)

openlog2 hostname port =
    do h <- connectTo hostname (PortNumber $ fromIntegral port)
       hSetBuffering h NoBuffering
       return h

syslog2 :: Handle -> String -> IO ()
syslog2 h msg = hPutStr h msg >> hFlush h

client = withSocketsDo $ do
  print "connecting as client..."
  h <- connectTo2 "localhost" diagPort
  hGetLine h >>= putStrLn
  hClose h

syslog :: SyslogHandle -> String -> IO ()
syslog syslogh msg =
    sendstr msg
    where -- Send until everything is done
          sendstr :: String -> IO ()
          sendstr [] = return ()
          sendstr omsg = do sent <- sendTo (slSocket syslogh) omsg
                                    (slAddress syslogh)
                            sendstr (genericDrop sent omsg)
          
closelog2 = hClose 

closelog :: SyslogHandle -> IO ()
closelog syslogh = sClose (slSocket syslogh)


