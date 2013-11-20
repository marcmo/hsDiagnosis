{-# LANGUAGE OverloadedStrings #-}
module Diag.Web.DataSerializer where

import Control.Applicative((<$>),(<*>))
import Control.Monad(mzero)
import Data.Attoparsec(parse,maybeResult)
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Aeson.Types as T
import qualified Data.ByteString.Lazy.Char8 as BL

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

testme = maybe (print "parse not successfull")
          (\v-> do
              let pd :: ChannelList
                  pd = case fromJSON v of
                          Success a -> a
                          Error s   -> error s
              print pd
              putStrLn $ "Encoded back: " ++ BL.unpack (encode pd))
          (maybeResult $ parse json testJson)

testJson = "[{\"name\":\"Joe\",\"id\":12},{\"name\":\"Hoe\",\"id\":13}]"


