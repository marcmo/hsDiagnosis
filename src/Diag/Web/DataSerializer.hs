{-# LANGUAGE OverloadedStrings #-}
module Diag.Web.DataSerializer where

import Control.Applicative((<$>),(<*>))
import Control.Monad(mzero)
import Data.Attoparsec(parse,maybeResult)
import Data.Aeson
import Data.Maybe(isJust,fromJust)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Aeson.Types as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

data ConnectRequest = CR B.ByteString deriving (Show)
instance ToJSON ConnectRequest where
     toJSON (CR n) = object ["channel" .= n]
instance FromJSON ConnectRequest where
    parseJSON (Object v) = CR <$> v .: "channel"

data Request = Incoming String String String
             | Outgoing String String String deriving (Show,Eq)
instance ToJSON Request where
    toJSON (Incoming a b c) = object ["response" .= object ["source" .= a, "target" .= b, "payload" .= c]]
    toJSON (Outgoing a b c) = object ["request" .= object ["source" .= a, "target" .= b, "payload" .= c]]
instance FromJSON Request where
    parseJSON j = do
      o <- parseJSON j -- takes care of JSON type check
      case HM.toList (o :: Object) of
        [("response", Object r)] -> Incoming <$> r .: "source" <*> r .: "target" <*> r .: "payload"
        [("request", Object r)] -> Outgoing <$> r .: "source" <*> r .: "target" <*> r .: "payload"
        _                      -> fail "Rule: unexpected format"

data ServerState = ServerState { _connected :: Bool, _output :: [Request]} deriving (Show)
instance ToJSON ServerState where
     toJSON (ServerState c o) = object ["connected" .= c, "output" .= o]
instance FromJSON ServerState where
    parseJSON (Object v) = ServerState <$> v .: "connected" <*> v .: "output"
    -- parseJSON (Array a) = D2 <$> mapM parseJSON (V.toList a)
  -- { 
  --   :connected => <true|false>,
  --   :output => [
  --     {:request => { :source => <source id>, :target => <target id>, :response => <data> } },
  --     {:response => { :source => <source id>, :target => <target id>, :response => <data> } },
  --     ...
  --   ]
  -- }
tt = "{\"output\":[{\"response\":{\"response\":\"abc\",\"source\":\"a\",\"target\":\"b\"}},{\"request\":{\"response\":\"ABC\",\"source\":\"A\",\"target\":\"B\"}}],\"connected\":true}"
testMe2 = fromJust $ maybeResult $ parse json testState

data Channel = Channel { _name :: String, _id :: Int } deriving (Show)
instance ToJSON Channel where
     toJSON (Channel n i) = object ["name" .= n, "id" .= i]
instance FromJSON Channel where
    parseJSON (Object v) = Channel <$>
                            v .: "name" <*>
                            v .: "id"
    parseJSON _ = mzero

data ChannelList = ChannelList [Channel] deriving (Show)
instance FromJSON ChannelList where
    parseJSON (Array a) = ChannelList <$> mapM parseJSON (V.toList a)
    parseJSON _ = mzero
instance ToJSON ChannelList where
  toJSON (ChannelList ps) = Array $ V.fromList [toJSON x | x <- ps]

testme ::  IO (Maybe ConnectRequest)
testme = maybe (print "parse not successfull" >> return Nothing)
          (\v-> do
              let pd = case fromJSON v of
                          Success a -> a
                          Error s   -> error s
              print pd
              putStrLn $ "Encoded back: " ++ BL.unpack (encode pd)
              return (Just pd))
          (maybeResult $ parse json testJson)

-- testJson = "[{\"name\":\"Joe\",\"id\":12},{\"name\":\"Hoe\",\"id\":13}]"
testJson = "{\"channel\":\"25\"}"
testState = "{\"connected\":\"true\",\"output\":[]}"
-- teststate ::  IO (Maybe ServerState)
-- teststate = maybe (print "parse not successfull" >> return Nothing)
--           (\v-> do
--               let pd = case fromJSON v of
--                           Success a -> a
--                           Error s   -> error s
--               print pd
--               putStrLn $ "Encoded back: " ++ BL.unpack (encode pd)
--               return (Just pd))
--           (maybeResult $ parse json testJson)
