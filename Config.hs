{-# LANGUAGE DeriveDataTypeable #-}
module Config
    (
       defaultConfig
      ,defaultDiagConfig
      ,defaultConfigFile
      ,diagConfigInFromFile
      ,mergeDiagConfigIns
      ,DiagConfigIn(..)
      ,hexIt
      ,inToDiagConfig
      ,loadDiagConfig
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

import DiagnosticConfig
import Data.Word
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T
import Data.Text.Read
import Numeric
import Control.Applicative
import System.Directory

defaultConfig :: IO CT.Config
defaultConfig = defaultConfigFile >>= configFromFile

defaultDiagConfig :: IO (Maybe DC.DiagConfig)
defaultDiagConfig = defaultConfig >>= diagConfigIn >>= return . inToDiagConfig

defaultConfigFile ::  IO String
defaultConfigFile = do home <- System.Directory.getUserDocumentsDirectory
                       return $ home ++ "/.hsDiagnosis/config.cfg"

configFromFile :: FilePath -> IO CT.Config
configFromFile file = fst <$> C.autoReload C.autoConfig [C.Required file]

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

lookup_ ::  CT.Configured a => String -> CT.Config -> IO (Maybe a)
lookup_ s c = fmap (maybe Nothing CT.convert) (lookupValue s c)

lookupHex_ :: Integral a => String -> CT.Config -> IO (Maybe a)
lookupHex_ s c = lookupValue s c >>=
  return . (maybe
            (Nothing)
            (\(CT.String h)-> either (const Nothing) (Just . fst) (hexadecimal h)))

lookupIp      = lookup_ "ip"
lookupPort    = lookup_ "port"
lookupTarget  = lookupHex_ "target"
lookupSource  = lookupHex_ "source"
lookupVerbose = lookup_ "verbose"
lookupTimeout = lookup_ "timeout"

defaultIp     = defaultConfig >>= lookupIp      :: IO (Maybe String)
defaultPort   = defaultConfig >>= lookupPort    :: IO (Maybe Int)
defaultTarget = defaultConfig >>= lookupTarget  -- :: IO (Maybe Word8)
defaultSource = defaultConfig >>= lookupSource  :: IO (Maybe Word8)
defaultVerbose= defaultConfig >>= lookupVerbose :: IO (Maybe Bool)
defaultTimeout= defaultConfig >>= lookupTimeout :: IO (Maybe Int)

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

-- mkConf :: Word8 -> String -> DC.DiagConfig
-- mkConf trg ip = DC.MkDiagConfig ip 6801 0xf4 trg debug_on standardDiagTimeout

diagConfigInFromFile :: FilePath -> IO DiagConfigIn
diagConfigInFromFile      f    = configFromFile f >>= diagConfigIn

diagConfigInGroupFromFile ::  FilePath -> String -> IO DiagConfigIn
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

loadDiagConfig :: FilePath -> IO (Maybe DC.DiagConfig)
loadDiagConfig f = do
  c <- diagConfigInFromFile f
  return $ inToDiagConfig c


--a = configFromFile defaultConfigFile
