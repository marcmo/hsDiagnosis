{-# LANGUAGE OverloadedStrings #-}

module WebInterface.Server where

import System.Hardware.Serialport

import WebInterface.SerialPort (serialSocket)
import Com.DiagClient (DiagConfig(MkDiagConfig), DiagnosisMessage, Word8, diagPayload, sendData)
import Config

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Network.WebSockets.Snap (runWebSocketsSnap)

import Control.Monad
import Control.Concurrent
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Text as T  (Text, split, strip, pack, unpack)
import Data.Maybe (fromJust)
import Data.Map (Map, lookup)
import Data.List (intercalate)
import Data.Char (toUpper)
import qualified Data.ByteString.UTF8 as BS (ByteString, toString, fromString)
import Numeric( showHex)


configHandler :: Snap ()
configHandler = do
  conf <- liftIO Config.defaultConfig
  ip   <- liftIO Config.defaultIp
  src  <- liftIO Config.defaultSource
  tgt  <- liftIO Config.defaultTarget
  sPath <- liftIO $ valueToString $ lookupValue Config.defaultConfig "serialPort"
  liftIO $ putStrLn $ showHex (fromJust src) ""
  writeBS $ BS.fromString $
  -- JSON:
     "{\"ip\" : \"" ++  fromJust ip ++ "\"," ++
      "\"src\" : \"" ++ map toUpper (showHex (fromJust src) "") ++ "\"," ++
      "\"tgt\" : \"" ++ map toUpper (showHex (fromJust tgt) "") ++ "\"," ++
      "\"serialPath\" : \"" ++ fromJust sPath  ++ "\"}"


diagMsgHandler :: Snap ()
diagMsgHandler = do
  params <- getPostParams
  let [ip, src, tgt, msg]  = map (lookupParam params) ["ip", "src", "tgt", "msg"]
      conf = MkDiagConfig ip 6801 (fromJust $ hexIt src) (fromJust $ hexIt tgt) False 5000
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
  ifTop (serveFile "WebInterface/resources/client.html")  <|>
    route [("sendDiagMsg", diagMsgHandler),
           ("defaultConfig", configHandler),
           ("jquery-1.6.3.min.js", serveFile "WebInterface/resources/jquery-1.6.3.min.js"),
           ("client.js", serveFile "WebInterface/resources/client.js"),
           ("client.css", serveFile "WebInterface/resources/client.css"),
           ("initSerialPort", runWebSocketsSnap  serialSocket)]


main :: IO ()
main = quickHttpServe site




