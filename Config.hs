{-# LANGUAGE DeriveDataTypeable #-}
module Config
    (
      defaultConfigFile,
      diagConfigInFromFile,
      mergeDiagConfigIns,
      DiagConfigIn(..),
      hexIt,
      inToDiagConfig,
      diagConfigInGroupFromFile
    )
where

import qualified Com.DiagClient as DC

import Data.Word
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T
import Numeric
import Control.Applicative


defaultConfigFile = "config.cfg"
-- maybe better(?):
-- defaultConfigFile = do home <- System.Directory.getUserDocumentsDirectory
--                        return $ home ++ "/.hsDiagnosis/config.cfg"

configFromFile :: FilePath -> IO CT.Config
configFromFile file = do (conf,_) <- C.autoReload C.autoConfig [C.Required file]
                         return conf

--------- not used from command line tool --------------------------------------
configGroupFromFile :: FilePath -> String -> IO CT.Config
configGroupFromFile file groupName = do c <- configFromFile file
                                        return $ C.subconfig (T.pack groupName) c

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
lookupVerbose conf = valueToBool   $ lookupValue conf "verbose"
lookupTimeout conf = valueToInt    $ lookupValue conf "timeout"

data DiagConfigIn = DiagConfigIn (Maybe String) (Maybe Int) (Maybe Word8) (Maybe Word8) (Maybe Bool) (Maybe Int)
  deriving (Show)

diagConfigIn :: IO CT.Config -> IO DiagConfigIn
diagConfigIn c =
  DiagConfigIn <$> lookupIp      c
               <*> lookupPort    c
               <*> lookupSource  c
               <*> lookupTarget  c
               <*> lookupVerbose c
               <*> lookupTimeout c

diagConfigInFromFile :: FilePath -> IO DiagConfigIn
diagConfigInFromFile      f    = diagConfigIn (configFromFile f)
diagConfigInGroupFromFile f g  = diagConfigIn (configGroupFromFile f g)

mergeDiagConfigIns :: DiagConfigIn -> DiagConfigIn -> DiagConfigIn
mergeDiagConfigIns (DiagConfigIn ip1 host1 source1 target1 verbose1 timeout1)
                 (DiagConfigIn ip2 host2 source2 target2 verbose2 timeout2) =
  DiagConfigIn (ip1      <|> ip2)
               (host1    <|> host2)
               (source1  <|> source2)
               (target1  <|> target2)
               (verbose1 <|> verbose2)
               (timeout1 <|> timeout2)

inToDiagConfig :: DiagConfigIn -> Maybe DC.DiagConfig
inToDiagConfig (DiagConfigIn (Just ip) (Just port) (Just target) (Just source) (Just verbose) (Just timeout)) =
  Just $ DC.MkDiagConfig ip port target source verbose timeout
inToDiagConfig _ = Nothing


--a = configFromFile defaultConfigFile
