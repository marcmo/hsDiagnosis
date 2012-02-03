{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import qualified Com.DiagClient as DC (host ,DiagConfig(..))
import DiagnosticConfig(conf)
import Control.Monad (when)
import Network.Socket
import Diagnoser.DiagScriptTester(runTestScript)
import Config(DiagExecuterArgs(..), mkConf,defaultConfigFile)

-- if a better help message and improved flags are required we better switch from CmdArgs to System.Console.GetOpt

diagConf = DiagExecuterArgs {
                    script = def &= args, -- &= typFile   &= help "diagnoser script to run",
                    config = def &= typFile   &= help ("configuration file, default points to \"" ++ defaultConfigFile ++ "\""),
                    ip     = def              &= help "ip address of ecu",
                    port   = def              &= help "ip address port",
                    source = def &= typ "Hex" &= help "default source, as hex e.g. f4",
                    target = def &= typ "Hex" &= help "default target, as hex e.g. 10",
                    debug  = def &= typ "Bool" &= help "default target, as hex e.g. 10",
                    timeout= def &= typ "Int" &= help "timeout for responses of diagnosis messages" 
                  } &= help "run diagnosis tests on a Diagnoser .skr file" 

main ::  IO ()
main = withSocketsDo $ do 
  dconf <- cmdArgs $ (modes [diagConf]
                      &= summary "DiagScripter 0.1.0, (c) Oliver Mueller 2010-2011")
                      &= verbosity
  c  <- mkConf dconf
  either (\argName -> putStrLn $ argName ++ " not specified. Must be set per flag or config file")
         (execute $ script dconf)
         c 

execute :: FilePath -> DC.DiagConfig -> IO ()
execute f conf@(DC.MkDiagConfig ip _ _ _ _ _ ) = do
    print $ "running script " ++ f ++ " on ip " ++ ip
    runTestScript f conf

