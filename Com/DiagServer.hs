module Com.DiagServer where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import qualified Network.Socket as NS
import System (getArgs)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import System.IO

type HandlerFunc = NS.SockAddr -> String -> IO ()

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ head args
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor handle
    sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    line <- hGetLine handle
    let cmd = words line
    print cmd
    case head cmd of
        ("echo") -> print "was echo" >> echoCommand handle cmd
        ("add") -> addCommand handle cmd
        _ -> print "s.th. else..." >> hPutStrLn handle "Unknown command"
    commandProcessor handle

echoCommand :: Handle -> [String] -> IO ()
echoCommand handle cmd = hPutStrLn handle (unwords $ tail cmd)

addCommand :: Handle -> [String] -> IO ()
addCommand handle cmd =
    hPutStrLn handle $ show $ read (cmd !! 1) + read (cmd !! 2)


serveDiagnosis :: String -> HandlerFunc -> IO ()
serveDiagnosis port handlerfunc = withSocketsDo $
    do addrinfos <- NS.getAddrInfo 
                    (Just (NS.defaultHints {NS.addrFlags = [NS.AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos
       sock <- NS.socket (NS.addrFamily serveraddr) NS.Stream NS.defaultProtocol
       NS.bindSocket sock (NS.addrAddress serveraddr)
       NS.listen sock 5
       lock <- newMVar ()
       procRequests lock sock

    where
          procRequests :: MVar () -> Socket -> IO ()
          procRequests lock mastersock = 
              do (connsock, clientaddr) <- NS.accept mastersock
                 handle lock clientaddr
                    "syslogtcpserver.hs: client connnected"
                 forkIO $ procMessages lock connsock clientaddr
                 procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: MVar () -> Socket -> NS.SockAddr -> IO ()
          procMessages lock connsock clientaddr =
              do connhdl <- NS.socketToHandle connsock ReadWriteMode
                 hSetBuffering connhdl NoBuffering
                 messages <- hGetContents connhdl
                 print "sending back..."
                 hPutStrLn connhdl "just an answere..." >> hFlush connhdl 
                 mapM_ (handle lock clientaddr) (lines messages)
                 print "closing..."
                 hClose connhdl
                 handle lock clientaddr 
                    "syslogtcpserver.hs: client disconnected"

          -- Lock the handler before passing data to it.
          handle :: MVar () -> NS.SockAddr -> String -> IO ()
          handle lock clientaddr msg =
              withMVar lock 
                 (\a -> handlerfunc clientaddr msg >> return a)

