{-# LANGUAGE OverloadedStrings #-}
module Main where

import Diag.Com.DiagClient (DiagConfig(MkDiagConfig), DiagnosisMessage, Word8, diagPayload, sendData)
import Diag.Config
import Diag.Web.DataSerializer
import Diag.Web.SessionSupport

import Snap.Core
import Snap.Extras.JSON
import Snap.Util.FileServe
import Snap.Http.Server
import Network.WebSockets.Snap (runWebSocketsSnap)
import qualified Data.Aeson as JSON

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Text as T  (split, strip, pack, unpack)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Char (toUpper)
import qualified Data.ByteString.UTF8 as BS (ByteString, toString, fromString)
import qualified Data.ByteString.Lazy as LBS
import Numeric( showHex)
import Diag.Util.Encoding

configHandler :: Snap ()
configHandler = do
  ip   <- liftIO defaultIp
  liftIO $ print ("using ip: " ++ show ip)
  src  <- liftIO Diag.Config.defaultSource
  tgt  <- liftIO Diag.Config.defaultTarget
  liftIO $ putStrLn $ showHex (fromJust src) ""
  writeBS $ BS.fromString $
  -- JSON:
     "{\"ip\" : \"" ++  fromJust ip ++ "\"," ++
      "\"src\" : \"" ++ map toUpper (showHex (fromJust src) "") ++ "\"," ++
      "\"tgt\" : \"" ++ map toUpper (showHex (fromJust tgt) "") ++ "\"}"


sendZgw d c = printData d >> sendData c d
showMsg = mapM_ (putStrLn . showAsHexString . diagPayload)
printData = putStrLn . showAsHexString
showPayload = mapM_ (printData . diagPayload)

diagMsgHandler :: Snap ()
diagMsgHandler = do
  liftIO $ print "inside diagMsgHandler............."
  params <- getPostParams
  let [ip, src, tgt, msg]  = map (lookupParam params) ["ip", "src", "tgt", "msg"]
      conf = MkDiagConfig ip 6801 (fromJust $ hexIt src) (fromJust $ hexIt tgt) True 5000
  liftIO $ print ("using conf: " ++ show conf)
  response  <- liftIO $ sendData conf (msgToWord8 msg)
  writeBS $  formatResponse response

  where
    msgToWord8 :: String -> [Word8]
    msgToWord8 msg = map (fromJust . hexIt . T.unpack . T.strip) $
                         T.split (\x -> ',' == x) $ T.pack msg
    formatResponse :: [DiagnosisMessage] -> BS.ByteString
    formatResponse r = BS.fromString $
                          map toUpper $
                          intercalate ", " $
                          map  (`showHex` "") $
                          diagPayload . head $ r

readBodyJson = getPostParams >>= return . fromJust . JSON.decode . LBS.fromStrict . head . M.keys
lookupParam :: Params -> BS.ByteString -> String
lookupParam params name = BS.toString . head . fromJust $ M.lookup name params

channelHandler :: Snap ()
channelHandler = do
  let channels = ChannelList [Channel "can" 25, Channel "ethernet" 42]
  writeBS $ LBS.toStrict $ JSON.encode channels

connectHandler ::  Snap ()
connectHandler = do
  params <- getPostParams
  con@(CR requestedChannel) <- readBodyJson :: Snap ConnectRequest
  liftIO $ putStrLn $ "inside connectHandler, params:" ++ show con
  liftIO $ putStrLn $ "requestedChannel:" ++ requestedChannel
  liftIO $ do
    env <- sessionEnv
    printEnv env
    defineVar env "connectedChannel" requestedChannel
    printEnv env
    env2 <- sessionEnv
    printEnv env2
  return ()

site :: Snap ()
site = do
  readRequestBody 100 >>= liftIO . print
  ifTop (serveFile "frontend/public/index.html")  <|>
    route [("sendDiagMsg", diagMsgHandler)
          ,("channels", channelHandler)
          ,("connect", connectHandler)
          ,("", serveDirectory "frontend/public")
          ,("defaultConfig", configHandler)] <|>
    dir "static" (serveDirectory "frontend/public")


main :: IO ()
main = quickHttpServe site




