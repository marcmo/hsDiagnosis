{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens.TH
import           Data.IORef
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet
import           Snap.Util.FileServe
-- import Snap.Core
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS (ByteString, toString, fromString)
import qualified Data.Map as M
import qualified Data.Set as S
import Numeric( showHex)
import Data.List (intercalate)
import Data.Text as T  (split, strip, pack, unpack)
import Data.Char (toUpper)

import Diag.Web.DataSerializer
import Diag.Com.DiagClient (DiagConfig(MkDiagConfig), DiagnosisMessage, Word8, diagPayload, sendData)
import Diag.Util.Encoding
import Diag.Config(hexIt)

data App = App
    { _heist       :: Snaplet (Heist App)
    , _channelId :: IORef (S.Set B.ByteString)
    }

makeLenses ''App
appInit :: SnapletInit App App
appInit = makeSnaplet "myapp" "My example application" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit "templates"
    addRoutes [ ("/hello", writeText "hello world")
              , ("/state", stateHandler)
              , ("/channels", channelHandler)
              , ("/connect", connectHandler)
              , ("/disconnect", disconnectHandler)
              , ("/request", requestHandler)
              , ("", serveDirectory "frontend/public")
              ]
    wrapSite (<|> heistServe)
    ref <- liftIO $ newIORef S.empty
    return $ App hs ref

namePage :: Handler b v ()
namePage = do
    mname <- getSnapletName
    writeText $ fromMaybe "This shouldn't happen" mname

readBodyJson ::  Handler App App ConnectRequest
readBodyJson = getPostParams >>= return . fromJust . JSON.decode . LBS.fromStrict . head . M.keys

readBodyJson2 ::  Handler App App ChannelRequest
readBodyJson2 = getPostParams >>= return . fromJust . JSON.decode . LBS.fromStrict . head . M.keys

stateHandler :: Handler App App ()
stateHandler = method GET getter <|> method POST setter
  where
    getter = writeBS "nyi"
    setter = do
        con@(CR requestedChannel) <- readBodyJson :: Handler App App ConnectRequest
        liftIO $ putStrLn $ "requestedChannel was:" ++ show con
        idRef <- gets _channelId
        registeredChannels <- liftIO $ readIORef idRef
        let ss = ServerState (S.member requestedChannel registeredChannels) []
        writeBS $ LBS.toStrict $ JSON.encode ss

connectHandler :: Handler App App ()
connectHandler = method GET getter <|> method POST setter
  where
    getter = writeBS "nyi"
    setter = do
        con@(CR requestedChannel) <- readBodyJson :: Handler App App ConnectRequest
        liftIO $ putStrLn $ "requestedChannel was:" ++ show con
        idRef <- gets _channelId
        liftIO $ modifyIORef' idRef (\x->S.insert requestedChannel x)

disconnectHandler :: Handler App App ()
disconnectHandler = method GET getter <|> method POST setter
  where
    getter = writeBS "nyi"
    setter = do
        con@(CR disconnectedChannel) <- readBodyJson :: Handler App App ConnectRequest
        liftIO $ putStrLn $ "disconnect channel:" ++ show con
        idRef <- gets _channelId
        liftIO $ modifyIORef' idRef (\x->S.delete disconnectedChannel x)

requestHandler :: Handler App App ()
requestHandler = method GET getter <|> method POST setter
  where
    getter = writeBS "GET for requestHandler not implemented"
    setter = do
        ps <- getPostParams
        liftIO $ putStrLn $ "requestHandler: postParameters were:" ++ show ((LBS.fromStrict . head . M.keys) ps)
        con@(ChannelRequest _id _request) <- readBodyJson2 :: Handler App App ChannelRequest
        liftIO $ putStrLn $ "requested for Channel :" ++ show _id ++ ", req:" ++ show _request
        idRef <- gets _channelId
        registeredChannels <- liftIO $ readIORef idRef
        if (S.member _id registeredChannels)
          then do
              let _ip = "localhost"
              liftIO $ putStrLn $ "Sending request to " ++ _ip
              let conf = MkDiagConfig _ip 6801 (fromJust $ hexIt (src _request)) (fromJust $ hexIt (tgt _request)) True 5000
              response  <- liftIO $ sendData conf (msgToWord8 (payload _request))
              writeBS $  formatResponse response
          else liftIO $ putStrLn "channel id not connected!"

msgToWord8 :: String -> [Word8]
msgToWord8 msg = map (fromJust . hexIt . T.unpack . T.strip) $
                    T.split (\x -> ',' == x) $ T.pack msg
formatResponse :: [DiagnosisMessage] -> BS.ByteString
formatResponse r = BS.fromString $
                      map toUpper $
                      intercalate ", " $
                      map  (`showHex` "") $
                      diagPayload . head $ r

channelHandler :: Handler App App ()
channelHandler = do
  let channels = ChannelList [Channel "can" 25, Channel "ethernet" 42]
  writeBS $ LBS.toStrict $ JSON.encode channels

instance HasHeist App where heistLens = subSnaplet heist
main :: IO ()
main = serveSnaplet defaultConfig appInit
-- {-# LANGUAGE OverloadedStrings #-}
-- module Main where
--
-- import Diag.Com.DiagClient (DiagConfig(MkDiagConfig), DiagnosisMessage, Word8, diagPayload, sendData)
-- import Diag.Config
-- import Diag.Web.DataSerializer
-- import Diag.Web.SessionSupport
--
-- import Snap.Core
-- import Snap.Extras.JSON
-- import Snap.Util.FileServe
-- import Snap.Http.Server
-- import Network.WebSockets.Snap (runWebSocketsSnap)
-- import qualified Data.Aeson as JSON
--
-- import Control.Applicative ((<|>))
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Text as T  (split, strip, pack, unpack)
-- import Data.Maybe (fromJust)
-- import qualified Data.Map as M
-- import Data.List (intercalate)
-- import Data.Char (toUpper)
-- import qualified Data.ByteString.UTF8 as BS (ByteString, toString, fromString)
-- import qualified Data.ByteString.Lazy as LBS
-- import Numeric( showHex)
-- import Diag.Util.Encoding
--
-- configHandler :: Snap ()
-- configHandler = do
--   ip   <- liftIO defaultIp
--   liftIO $ print ("using ip: " ++ show ip)
--   src  <- liftIO Diag.Config.defaultSource
--   tgt  <- liftIO Diag.Config.defaultTarget
--   liftIO $ putStrLn $ showHex (fromJust src) ""
--   writeBS $ BS.fromString $
--   -- JSON:
--      "{\"ip\" : \"" ++  fromJust ip ++ "\"," ++
--       "\"src\" : \"" ++ map toUpper (showHex (fromJust src) "") ++ "\"," ++
--       "\"tgt\" : \"" ++ map toUpper (showHex (fromJust tgt) "") ++ "\"}"
--
--
-- sendZgw d c = printData d >> sendData c d
-- showMsg = mapM_ (putStrLn . showAsHexString . diagPayload)
-- printData = putStrLn . showAsHexString
-- showPayload = mapM_ (printData . diagPayload)
--
-- diagMsgHandler :: Snap ()
-- diagMsgHandler = do
--   liftIO $ print "inside diagMsgHandler............."
--   params <- getPostParams
--   let [ip, src, tgt, msg]  = map (lookupParam params) ["ip", "src", "tgt", "msg"]
--       conf = MkDiagConfig ip 6801 (fromJust $ hexIt src) (fromJust $ hexIt tgt) True 5000
--   liftIO $ print ("using conf: " ++ show conf)
--   response  <- liftIO $ sendData conf (msgToWord8 msg)
--   writeBS $  formatResponse response
--
--   where
--     msgToWord8 :: String -> [Word8]
--     msgToWord8 msg = map (fromJust . hexIt . T.unpack . T.strip) $
--                          T.split (\x -> ',' == x) $ T.pack msg
--     formatResponse :: [DiagnosisMessage] -> BS.ByteString
--     formatResponse r = BS.fromString $
--                           map toUpper $
--                           intercalate ", " $
--                           map  (`showHex` "") $
--                           diagPayload . head $ r
--
-- readBodyJson = getPostParams >>= return . fromJust . JSON.decode . LBS.fromStrict . head . M.keys
-- lookupParam :: Params -> BS.ByteString -> String
-- lookupParam params name = BS.toString . head . fromJust $ M.lookup name params
--
-- channelHandler :: Snap ()
-- channelHandler = do
--   let channels = ChannelList [Channel "can" 25, Channel "ethernet" 42]
--   writeBS $ LBS.toStrict $ JSON.encode channels
--
-- connectHandler ::  Snap ()
-- connectHandler = do
--   params <- getPostParams
--   con@(CR requestedChannel) <- readBodyJson :: Snap ConnectRequest
--   liftIO $ putStrLn $ "inside connectHandler, params:" ++ show con
--   liftIO $ putStrLn $ "requestedChannel:" ++ requestedChannel
--   liftIO $ do
--     env <- sessionEnv
--     printEnv env
--     defineVar env "connectedChannel" requestedChannel
--     printEnv env
--     env2 <- sessionEnv
--     printEnv env2
--   return ()
--
-- site :: Snap ()
-- site = do
--   readRequestBody 100 >>= liftIO . print
--   ifTop (serveFile "frontend/public/index.html")  <|>
--     route [("sendDiagMsg", diagMsgHandler)
--           ,("channels", channelHandler)
--           ,("connect", connectHandler)
--           ,("", serveDirectory "frontend/public")
--           ,("defaultConfig", configHandler)] <|>
--     dir "static" (serveDirectory "frontend/public")
--
--
-- main :: IO ()
-- main = quickHttpServe site
--
--
--
--
