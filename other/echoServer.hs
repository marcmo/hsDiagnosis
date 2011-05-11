-- Echo server program
module Main where

import Control.Monad (unless,when)
import Network.Socket hiding (recv)
import qualified Data.ByteString as S
import Data.Word(Word8)
import Control.Concurrent(threadDelay)
import Data.List
import Numeric
import Network.Socket.ByteString (recv, sendAll)

main = withSocketsDo $
    do addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just "6666")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       bindSocket sock (addrAddress serveraddr)
       listen sock 1
       loop sock

    where
      loop s = do
        (connSock, _) <- accept s
        talk connSock
        loop s

      whenM a b = a >>= (flip when) b
      talk :: Socket -> IO ()
      talk connSock = do 
             putStrLn "now we are talking..."
             whenM (sIsConnected connSock) (putStrLn "connected!")
             whenM (sIsReadable connSock) (putStrLn "readable!")
             whenM (sIsWritable connSock) (putStrLn "writable!")
             msg <- recv connSock 1024
             print $ "received over the wire: " ++ (show msg)
             unless (S.null msg) $ do
              threadDelay(500*1000)
              sendAll connSock $ replyTo msg
              putStrLn "sent back response, starting to listen again..."
              talk connSock
      replyTo m = S.reverse m

