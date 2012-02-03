{-# LANGUAGE DeriveDataTypeable #-}
module Config
     (defaultConfigFile,
      DiagExecuterArgs(..),
      mkConf
     ) 

where

import qualified Com.DiagClient as DC

import Data.Word
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Control.Monad
import qualified Data.Text as T
import Util.Encoding
import Numeric
import Data.Maybe
import Control.Applicative

import Data.Typeable
import Data.Data


defaultConfigFile = "config.cfg"
-- maybe better(?):
-- defaultConfigFile = do home <- System.Directory.getUserDocumentsDirectory 
--                        return $ home ++ "/.hsDiagnosis/config.cfg"

defaultConfig :: IO CT.Config
defaultConfig = confFromFile defaultConfigFile 

confFromFile :: FilePath -> IO CT.Config
confFromFile file = do (conf,_) <- C.autoReload C.autoConfig [C.Required file]
                       return conf

--------- not used from command line tool --------------------------------------
configGroup :: FilePath -> String -> IO CT.Config
configGroup file groupName = do c <- confFromFile file
                                return $ C.subconfig (T.pack groupName) c

group1 = configGroup defaultConfigFile "group1"
--------------------------------------------------------------------------------

hexIt :: String -> Maybe Word8
hexIt s = case readHex s of
            [(h,[])]  -> Just h    
            _         -> Nothing


lookupValue :: IO CT.Config -> String -> IO (Maybe CT.Value)
lookupValue conf name = do c <- conf
                           C.lookup c (T.pack name) 


valueToInt :: IO (Maybe CT.Value) -> IO (Maybe Int)
valueToInt v = do o <- v;  return $ o  >>= CT.convert

valueToString :: IO (Maybe CT.Value) -> IO (Maybe String)
valueToString v = do o <- v;  return $  o >>=  CT.convert

valueToBool :: IO (Maybe CT.Value) -> IO (Maybe Bool)
valueToBool v = do o <- v;  return $  o >>=  CT.convert

valueToHex :: IO (Maybe CT.Value) -> IO (Maybe Word8)
valueToHex v = do o <- v; return $  o >>=  CT.convert  >>= hexIt


lookupIp      conf = valueToString $ lookupValue conf "ip"
lookupPort    conf = valueToInt    $ lookupValue conf "port"  
lookupTarget  conf = valueToHex    $ lookupValue conf "target"
lookupSource  conf = valueToHex    $ lookupValue conf "source"
lookupDebug   conf = valueToBool   $ lookupValue conf "debug"
lookupTimeout conf = valueToInt    $ lookupValue conf "standart-timeout"

defaultIp     = lookupIp      defaultConfig
defaultPort   = lookupPort    defaultConfig
defaultTarget = lookupTarget  defaultConfig
defaultSource = lookupSource  defaultConfig
defaultDebug  = lookupDebug   defaultConfig
defaultTimeout= lookupTimeout defaultConfig


mkConf :: DiagExecuterArgs -> IO (Either String DC.DiagConfig)
mkConf (DiagExecuterArgs _ configFile ip port source target debug timeout) = do
  let c = maybe defaultConfig confFromFile configFile
  iD  <- lookupIp      c
  pD  <- lookupPort    c
  tD  <- lookupTarget  c
  sD  <- lookupSource  c
  dD  <- lookupDebug   c
  oD  <- lookupTimeout c
  return $ DC.MkDiagConfig <$> mergeConfigItems "ip"      ip      iD
                           <*> mergeConfigItems "port"    port    pD
                           <*> mergeConfigItems "source"  (source >>= hexIt) sD
                           <*> mergeConfigItems "target"  (target >>= hexIt) tD
                           <*> mergeConfigItems "debug"   debug   dD
                           <*> mergeConfigItems "timeout" timeout oD  
-- drawback: returns only the the first undefined element
-- returning a combination of all undefined elements would improve the error message

mergeConfigItems :: String -> Maybe a -> Maybe a -> Either String a
mergeConfigItems name majorItem minorItem = 
  case majorItem <|> minorItem of
    Nothing  -> Left name
    (Just a) -> Right a  



data DiagExecuterArgs = DiagExecuterArgs {
  script :: String,
  config :: Maybe FilePath,
  ip     :: Maybe String,
  port   :: Maybe Int,
  source :: Maybe String,
  target :: Maybe String,
  debug  :: Maybe Bool,
  timeout :: Maybe Int
} deriving (Show, Data, Typeable)

