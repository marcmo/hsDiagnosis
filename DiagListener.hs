import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import DiagnosticConfig
import Text.Printf
import DiagClient
import Foreign
import Foreign.C.String


serveLog :: HandlerFunc -> IO ()
serveLog handlerfunc = withSocketsDo $ do
       addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       bindSocket sock (addrAddress serveraddr)
       listen sock 5
       lock <- newMVar ()
       procRequests lock sock

    where
          -- | Process incoming connection requests
          procRequests :: MVar () -> Socket -> IO ()
          procRequests lock mastersock = 
              do (connsock, clientaddr) <- accept mastersock
                 handle lock clientaddr
                    "syslogtcpserver.hs: client connnected"
                 forkIO $ procMessages lock connsock clientaddr
                 procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: MVar () -> Socket -> SockAddr -> IO ()
          procMessages lock connsock clientaddr =
              do connhdl <- socketToHandle connsock ReadMode
                 hSetBuffering connhdl LineBuffering
                 messages <- hGetContents connhdl
                 mapM_ (handle lock clientaddr) (lines messages)
                 hClose connhdl
                 handle lock clientaddr 
                    "syslogtcpserver.hs: client disconnected"

          -- Lock the handler before passing data to it.
          handle :: MVar () -> HandlerFunc
          -- This type is the same as
          -- handle :: MVar () -> SockAddr -> String -> IO ()
          handle lock clientaddr msg =
              withMVar lock 
                 (\a -> handlerfunc clientaddr msg >> return a)

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = 
    putStrLn $ "From " ++ show addr ++ ": " ++ msg

simpelListen = do
    print "1"
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    print "2"
    let serveraddr = head addrinfos
    print "3"
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    print "4"
    setSocketOption sock KeepAlive 1
    print "5"
    connect sock (addrAddress serveraddr)
    print "6"
    h <- socketToHandle sock ReadMode
    print "7"
    hSetBuffering h NoBuffering
    print "8"
    listenSimple h
 
write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t
 
listenSimple :: Handle -> IO ()
listenSimple h = forever $ do
    c <- hGetChar h
    print $ "received: " ++ [c]
    -- putStrLn s
  where
    forever a = do a; forever a

easyListen :: IO()
easyListen = do
    -- addrinfos <- getAddrInfo 
    --             (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    --             Nothing (Just port)
    -- let serveraddr = head addrinfos
    -- sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    -- bindSocket sock (addrAddress serveraddr)
    -- listen sock 5
    -- lock <- newMVar ()
    -- forkIO $ procResponse lock sock 
    -- print "after forking"
    addrinfos2 <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr2 = head addrinfos2
    print serveraddr2
    sock2 <- socket (addrFamily serveraddr2) Stream defaultProtocol
    print "socket called"
    setSocketOption sock2 KeepAlive 1
    connect sock2 (addrAddress serveraddr2)
    print "socket connected"
    h <- socketToHandle sock2 ReadMode
    hSetBuffering h LineBuffering
    inputAvailable <- hWaitForInput h 10000
    print $ "input was available:" ++ (show inputAvailable)
    b <- mallocBytes 1000
    bytesRead <- hGetBufNonBlocking h b 1000
    print $ show bytesRead
    -- listenSimple h
    -- resp <- hGetContents h
    -- print $ (show $ concat $ parsePayload resp)
    print $ "bytes read: " ++ (show bytesRead)
    res <- peekCStringLen (b,bytesRead)
    print res
    let (Just rmsg) = bytes2msg res
    print rmsg
    print "after response"
    hClose h
