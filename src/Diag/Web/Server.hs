{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Diag.Web.SerialPort (serialSocket)
import Diag.Com.DiagClient (DiagConfig(MkDiagConfig), DiagnosisMessage, Word8, diagPayload, sendData)
import Diag.Config

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Network.WebSockets.Snap (runWebSocketsSnap)

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Text as T  (split, strip, pack, unpack)
import Data.Maybe (fromJust)
import Data.Map (lookup)
import Data.List (intercalate)
import Data.Char (toUpper)
import qualified Data.ByteString.UTF8 as BS (ByteString, toString, fromString)
import Numeric( showHex)
import Diag.Util.Encoding
-- import DiagnosticConfig


configHandler :: Snap ()
configHandler = do
  ip   <- liftIO defaultIp
  liftIO $ print ("using ip: " ++ show ip)
  src  <- liftIO Diag.Config.defaultSource
  tgt  <- liftIO Diag.Config.defaultTarget
  sPath <- liftIO $ valueToString $ Diag.Config.defaultConfig >>= lookupValue "serialPort"
  liftIO $ putStrLn $ showHex (fromJust src) ""
  writeBS $ BS.fromString $
  -- JSON:
     "{\"ip\" : \"" ++  fromJust ip ++ "\"," ++
      "\"src\" : \"" ++ map toUpper (showHex (fromJust src) "") ++ "\"," ++
      "\"tgt\" : \"" ++ map toUpper (showHex (fromJust tgt) "") ++ "\"," ++
      "\"serialPath\" : \"" ++ fromJust sPath  ++ "\"}"


sendZgw d c = printData d >> sendData c d
showMsg = mapM_ (putStrLn . showAsHexString . diagPayload)
extendedSession = sendZgw [0x10, 0x3]
printData = putStrLn . showAsHexString
showPayload = mapM_ (printData . diagPayload)
signatureLength = 192
certificateFile = "../test.der"
securityAccessRequestSeed = 0x3
securityAccessSendKey = 0x4


createSignature _ = [0..signatureLength-1]

diagMsgHandler :: Snap ()
diagMsgHandler = do
  params <- getPostParams
  let [ip, src, tgt, msg]  = map (lookupParam params) ["ip", "src", "tgt", "msg"]
      conf = MkDiagConfig ip 6801 (fromJust $ hexIt src) (fromJust $ hexIt tgt) True 5000
  liftIO $ print ("using conf: " ++ show conf)
  response  <- liftIO $ sendData conf (msgToWord8 msg)
  writeBS $  formatResponse response

  where
    lookupParam :: Params -> BS.ByteString -> String
    lookupParam params name = BS.toString . head . fromJust $
                                 Data.Map.lookup name params
    msgToWord8 :: String -> [Word8]
    msgToWord8 msg = map (fromJust . hexIt . T.unpack . T.strip) $
                         T.split (\x -> ',' == x) $ T.pack msg
    formatResponse :: [DiagnosisMessage] -> BS.ByteString
    formatResponse r = BS.fromString $
                          map toUpper $
                          intercalate ", " $
                          map  (`showHex` "") $
                          diagPayload . head $ r

site :: Snap ()
site =
  ifTop (serveFile "resources/client.html")  <|>
    route [("sendDiagMsg", diagMsgHandler),
           ("defaultConfig", configHandler),
           ("jquery-1.6.3.min.js", serveFile "WebInterface/resources/jquery-1.6.3.min.js"),
           ("client.js", serveFile "WebInterface/resources/client.js"),
           ("client.css", serveFile "WebInterface/resources/client.css")]
           -- ("initSerialPort", runWebSocketsSnap  serialSocket)]


main :: IO ()
main = quickHttpServe site




