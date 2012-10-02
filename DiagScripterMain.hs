{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import qualified Com.DiagClient as DC (DiagConfig(..))
import Network.Socket
import Diagnoser.DiagScriptTester(runTestScript)
import Config
import qualified Data.List as DL (intercalate)

-- if a better help message and improved flags are required we better switch from CmdArgs to System.Console.GetOpt

data DiagExecuter = DiagExecuter {
  script :: String,
  config :: FilePath,
  ip     :: Maybe String,
  port   :: Maybe Int,
  source :: Maybe String,
  target :: Maybe String,
  verbose:: Maybe Bool,
  timeout:: Maybe Int
} deriving (Show, Data, Typeable)


diagExecuter = DiagExecuter {
                    script = def &= args,
                    config = defaultConfigFile &= typFile &= help ("configuration file, default points to \"" ++ defaultConfigFile ++ "\""),
                    ip     = def              &= help "ip address of ecu", -- what is an ecu
                    port   = def              &= help "ip address port",
                    source = def &= typ "Hex" &= help "default source, as hex e.g. f4",
                    target = def &= typ "Hex" &= help "default target, as hex e.g. 10",
                    verbose= def &= typ "Bool"&= help "verbose output",
                    timeout= def &= typ "Int" &= help "timeout for responses of diagnosis messages"
                  } &= help "run diagnosis tests on a Diagnoser .skr file"

main ::  IO ()
main = withSocketsDo $ do
  arguments <- cmdArgs $ diagExecuter
                      &= summary "DiagScripter 0.1.0, (c) Oliver Mueller 2010-2011"
  fileConfig <-  diagConfigInFromFile (config arguments)
  let argsConfig = DiagConfigIn (ip arguments)
                                (port arguments)
                                (source arguments >>= hexIt)
                                (target arguments >>= hexIt)
                                (verbose arguments)
                                (timeout arguments)
      finalConfig = mergeDiagConfigIns fileConfig argsConfig
  executeOrError (script arguments) finalConfig

executeOrError :: FilePath -> DiagConfigIn -> IO ()
executeOrError scriptPath c =
  maybe (missingItemsError $ missingConfigItems c)
        (execute scriptPath)
        (inToDiagConfig c)

missingConfigItems :: DiagConfigIn -> String
missingConfigItems (DiagConfigIn _ip _port _source _target _verbose _timeout) =
   DL.intercalate ", "
           ([] <: (_ip, "ip")
               <: (_port, "port")
               <: (_source, "source")
               <: (_target, "target")
               <: (_verbose, "verbose")
               <: (_timeout, "timeout"))
  where xs <: (Nothing, _name) = let x = "\"" ++ _name ++ "\"" in
                                    xs ++ [x]
        xs <: _ = xs

missingItemsError :: String -> IO ()
missingItemsError items = putStrLn $
  "Error: The necessary config items " ++ items ++ " are not specified\n" ++
  "set them in the configuration file or via the command line flags"

execute :: FilePath -> DC.DiagConfig -> IO ()
execute f conf@(DC.MkDiagConfig _ip _ _ _ _ _ ) = do
    print $ "running script " ++ f ++ " on ip " ++ _ip
    runTestScript f conf
