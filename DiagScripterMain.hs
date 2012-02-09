{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import qualified Com.DiagClient as DC (host ,DiagConfig(..))
import Control.Monad (when)
import Network.Socket
import Diagnoser.DiagScriptTester(runTestScript)
import Config 
import qualified Config as Conf
import qualified Data.List as DL (intersperse)
import Data.Maybe

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
  args <- cmdArgs $ diagExecuter
                      &= summary "DiagScripter 0.1.0, (c) Oliver Mueller 2010-2011"
  fileConfig <-  diagConfigInFromFile (config args)
  let argsConfig = DiagConfigIn (ip args) 
                                (port args) 
                                (source args >>= hexIt)
                                (target args >>= hexIt)
                                (verbose args) 
                                (timeout args)
      finalConfig = mergeDiagConfigIns fileConfig argsConfig
  executeOrError (script args) finalConfig

executeOrError :: FilePath -> DiagConfigIn -> IO ()
executeOrError scriptPath c = 
  maybe (missingItemsError $ missingConfigItems c)
        (execute scriptPath)
        (inToDiagConfig c)

missingConfigItems :: DiagConfigIn -> String
missingConfigItems c@(DiagConfigIn ip port source target verbose timeout) = 
   concat $ DL.intersperse ", "
           ([] <: (ip, "ip") 
               <: (port, "port") 
               <: (source, "source") 
               <: (target, "target") 
               <: (verbose, "verbose") 
               <: (timeout, "timeout"))
  where xs <: (Nothing, name) = let x = "\"" ++ name ++ "\"" in 
                                    xs ++ [x] 
        xs <: (Just b, name ) = xs

missingItemsError :: String -> IO ()
missingItemsError items = putStrLn $
  "Error: The necessary config items " ++ items ++ " are not specified\n" ++
  "set them in the configuration file or via the command line flags"

execute :: FilePath -> DC.DiagConfig -> IO ()
execute f conf@(DC.MkDiagConfig ip _ _ _ _ _ ) = do
    print $ "running script " ++ f ++ " on ip " ++ ip
    runTestScript f conf