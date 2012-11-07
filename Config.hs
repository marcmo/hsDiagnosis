{-# LANGUAGE DeriveDataTypeable #-}
module Config
    (
       defaultConfigFile
      ,defaultConfig
      ,diagConfigInFromFile
      ,mergeDiagConfigIns
      ,DiagConfigIn(..)
      ,hexIt
      ,inToDiagConfig
      ,diagConfigInGroupFromFile
      ,valueToString
      ,lookupValue
      ,defaultIp
      ,defaultSource
      ,defaultTarget
      ,defaultPort
      ,defaultVerbose
      ,defaultTimeout
    )
where

import qualified Com.DiagClient as DC

import Data.Word
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T
import Numeric
import Control.Applicative

defaultConfig :: IO CT.Config
defaultConfig = configFromFile defaultConfigFile
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

lookupValue :: String -> CT.Config -> IO (Maybe CT.Value)
lookupValue name conf = C.lookup conf (T.pack name)

valueToString :: IO (Maybe CT.Value) -> IO (Maybe String)
valueToString = fmap $ (=<<) CT.convert

lookup_ s c = fmap (maybe Nothing CT.convert) (lookupValue s c)
lookupIp      = lookup_ "ip"
lookupPort    = lookup_ "port"
lookupTarget  = lookup_ "target"
lookupSource  = lookup_ "source"
lookupVerbose = lookup_ "verbose"
lookupTimeout = lookup_ "timeout"

defaultIp     = defaultConfig >>= lookupIp
defaultPort   = defaultConfig >>= lookupPort
defaultTarget = defaultConfig >>= lookupTarget
defaultSource = defaultConfig >>= lookupSource
defaultVerbose= defaultConfig >>= lookupVerbose
defaultTimeout= defaultConfig >>= lookupTimeout
data DiagConfigIn = DiagConfigIn (Maybe String) (Maybe Int) (Maybe Word8) (Maybe Word8) (Maybe Bool) (Maybe Int)
  deriving (Show)

diagConfigIn :: CT.Config -> IO DiagConfigIn
diagConfigIn c =
  DiagConfigIn <$> lookupIp      c
               <*> lookupPort    c
               <*> lookupSource  c
               <*> lookupTarget  c
               <*> lookupVerbose c
               <*> lookupTimeout c

mkConf :: Word8 -> String -> DiagConfig
mkConf trg ip = MkDiagConfig ip 6801 0xf4 trg debug_on standardDiagTimeout
zgwConfig = mkConfig 

diagConfigInFromFile :: FilePath -> IO DiagConfigIn
diagConfigInFromFile      f    = configFromFile f >>= diagConfigIn
diagConfigInGroupFromFile f g  = configGroupFromFile f g >>= diagConfigIn

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
