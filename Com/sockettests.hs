import Network.Socket
import Network.BSD
import System.IO (Handle, IOMode(ReadWriteMode), hPutStrLn, hGetLine, hClose)
import Control.Monad (liftM)
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import System

connectTo :: String -> Int -> IO Handle
connectTo host port_ = do
      let port = toEnum port_
      sock <- socket AF_INET Stream 0
      addrs <- liftM hostAddresses $ getHostByName host
      if null addrs then error $ "no such host : " ++ host else return ()
      connect sock $ SockAddrInet port (head addrs)
      handle <- socketToHandle sock ReadWriteMode
      return handle

listenAt :: Int -> (Handle -> IO ()) -> IO ()
listenAt port_ f = do
      let port = toEnum port_
      lsock <- socket AF_INET Stream 0
      bindSocket lsock $ SockAddrInet port iNADDR_ANY
      listen lsock sOMAXCONN
      loop lsock `finally` sClose lsock
   where
      loop lsock = do
      (sock,SockAddrInet _ _) <- accept lsock
      handle <- socketToHandle sock ReadWriteMode
      f handle
      loop lsock

server = withSocketsDo $ do
  print "server listening ..."
  listenAt 12345 (\h -> forkIO (do
      putStrLn "connection."
      hPutStrLn h "Hello, and Goodbye!"
   `finally` hClose h) >> return ())

client = withSocketsDo $ do
  print "connecting as client..."
  h <- connectTo "localhost" 12345
  hGetLine h >>= putStrLn
  hClose h

main = do
  [a] <- getArgs
  if a == "1" 
  	then server
  	else client

