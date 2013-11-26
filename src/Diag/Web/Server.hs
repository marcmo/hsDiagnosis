{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens.TH
import           Snap
import           Snap.Snaplet.Heist
-- import           Snap.Snaplet
import           Snap.Util.FileServe

import           Data.IORef
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS (ByteString, fromString)
import qualified Data.Map as M
import qualified Data.Set as S
import           Numeric( showHex)
import           Data.Text as T  (split, strip, pack, unpack)
import           Data.Char (toUpper)

import           Diag.Web.DataSerializer
import           Diag.Com.DiagClient (DiagConfig(MkDiagConfig), DiagnosisMessage, Word8, diagPayload, sendData)
import           Diag.Config(hexIt)

data App = App
    { _heist     :: Snaplet (Heist App)
    , _channelId :: IORef (S.Set B.ByteString)
    }

makeLenses ''App
appInit :: SnapletInit App App
appInit = makeSnaplet "diagapp" "Webserver for diangosis frontend" Nothing $ do
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
        CR requestedChannel <- readBodyJson :: Handler App App ConnectRequest
        idRef <- gets _channelId
        registeredChannels <- liftIO $ readIORef idRef
        let ss = ServerState (S.member requestedChannel registeredChannels) []
        writeBS $ LBS.toStrict $ JSON.encode ss

connectHandler :: Handler App App ()
connectHandler = method GET getter <|> method POST setter
  where
    getter = writeBS "nyi"
    setter = do
        CR requestedChannel <- readBodyJson :: Handler App App ConnectRequest
        liftIO $ putStrLn $ "connect request for channel " ++ show requestedChannel
        idRef <- gets _channelId
        liftIO $ modifyIORef' idRef (S.insert requestedChannel)

disconnectHandler :: Handler App App ()
disconnectHandler = method GET getter <|> method POST setter
  where
    getter = writeBS "nyi"
    setter = do
        con@(CR disconnectedChannel) <- readBodyJson :: Handler App App ConnectRequest
        liftIO $ putStrLn $ "disconnect channel:" ++ show con
        idRef <- gets _channelId
        liftIO $ modifyIORef' idRef (S.delete disconnectedChannel)

requestHandler :: Handler App App ()
requestHandler = method GET getter <|> method POST setter
  where
    getter = writeBS "GET for requestHandler not implemented"
    setter = do
        ps <- getPostParams
        liftIO $ putStrLn $ "requestHandler: postParameters were:" ++ show ((LBS.fromStrict . head . M.keys) ps)
        ChannelRequest _id _request <- readBodyJson2 :: Handler App App ChannelRequest
        liftIO $ putStrLn $ "requested for Channel :" ++ show _id ++ ", req:" ++ show _request
        idRef <- gets _channelId
        registeredChannels <- liftIO $ readIORef idRef
        if S.member _id registeredChannels
          then do
              let _ip = "localhost"
              liftIO $ putStrLn $ "Sending request to " ++ _ip
              let conf = MkDiagConfig _ip 6801 (fromJust $ hexIt (src _request)) (fromJust $ hexIt (tgt _request)) True 5000
              response  <- liftIO $ sendData conf (msgToWord8 (payload _request))
              writeBS $  formatResponse response
          else liftIO $ putStrLn "channel id not connected!"

msgToWord8 :: String -> [Word8]
msgToWord8 msg = map (fromJust . hexIt . T.unpack . T.strip) $
                    T.split (\x -> ' ' == x) $ T.pack msg
formatResponse :: [DiagnosisMessage] -> BS.ByteString
formatResponse r = BS.fromString $
                      map toUpper $
                      unwords $
                      map  (`showHex` "") $
                      diagPayload . head $ r

channelHandler :: Handler App App ()
channelHandler = do
  let channels = ChannelList [Channel "can" 25, Channel "ethernet" 42]
  writeBS $ LBS.toStrict $ JSON.encode channels

instance HasHeist App where heistLens = subSnaplet heist

main :: IO ()
main = serveSnaplet defaultConfig appInit

