-- Echo server program
module Main where

import Control.Monad (unless)
import Network.Socket hiding (recv)
import qualified Data.ByteString as S
import Network.Socket.ByteString (recv, sendAll)
import Com.DiagMessage

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just "6801")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       bindSocket sock (addrAddress serveraddr)
       listen sock 1
       loop sock

    where
      loop s = do
        (conn, _) <- accept s
        talk conn
        loop s
        -- sClose conn
        -- sClose sock
      talk :: Socket -> IO ()
      talk conn = do 
             putStrLn "now we are talking..."
             msg <- recv conn 1024
             let hsfzMsg = bytes2msg msg
             unless (S.null msg) $ sendAll conn msg >> print msg >> talk conn

