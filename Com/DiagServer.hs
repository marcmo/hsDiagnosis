module Com.DiagServer where

import Com.DiagMessage
import Com.HSFZMessage
import Com.DiagBase
import Data.Bits
import Network.Socket
import Network.BSD
-- import Network(PortID(PortNumber),listenOn)
import Data.List
import Control.Exception
import Text.Printf(printf)


type HandlerFunc = SockAddr -> String -> IO ()

main = serveDiagnosis plainHandler
serveDiagnosis :: HandlerFunc -> IO ()
serveDiagnosis handlerfunc = withSocketsDo $
    do let c = MkDiagConfig "localhost" 6801 0xf4 0x40 True
       sock <- connectMe c
       procMessages sock
    where procMessages sock =
              do (msg, _, addr) <- recvFrom sock 1024
                 handlerfunc addr msg
                 procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = 
    putStrLn $ "From " ++ show addr ++ ": " ++ msg


connectMe :: DiagConfig -> IO Socket
connectMe c = notify $ con c
  where
    notify
      | verbose c = bracket_ (printf "Connecting to %s ... " (host c)) (putStrLn "done.")
      | otherwise = bracket_ (return ()) (return ())


con c =
   do addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just $ show $ port c)
      let serveraddr = head addrinfos
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      setSocketOption sock KeepAlive 1
      bindSocket sock (addrAddress serveraddr)
      print $ "listening on port:" ++ show diagPort ++ ", serveraddr:" ++ show (head addrinfos)
      return sock

con2 c = 
    do addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just (show diagPort))
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
       bindSocket sock (addrAddress serveraddr)
       print $ "listening on port:" ++ show diagPort ++ ", serveraddr:" ++ show (head addrinfos)
       return sock
