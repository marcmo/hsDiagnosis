-- Echo server program
module Main where

import Control.Monad (unless)
import Network.Socket hiding (recv)
import qualified Data.ByteString as S
import Network.Socket.ByteString (recv, sendAll)
import Com.DiagMessage
import Com.HSFZMessage

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
             print msg
             let hsfzMsg = bytes2msg msg
             print hsfzMsg
             unless (S.null msg) $ do
              sendAll conn $ replyTo hsfzMsg
              print $ "received over the wire: " ++ (showBinString msg)
              print hsfzMsg
              talk conn
      replyTo :: Maybe HSFZMessage -> S.ByteString
      replyTo (Just m) = msg2ByteString responseHsfz
        where dmsg = hsfz2diag m
              payload = diagPayload dmsg
              responsePayload = (head payload + 0x40):(tail payload)
              response = DiagnosisMessage (diagTarget dmsg) (diagSource dmsg) responsePayload
              responseHsfz = diag2hsfz response DataBit
      replyTo _ = error "not a valid message"

