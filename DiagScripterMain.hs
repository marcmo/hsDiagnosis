{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import Com.DiagClient(sendData)
import DiagnosticConfig
import Control.Monad (when)
import Network.Socket
import Script.ErrorMemory
import Script.LoggingFramework
import Test.DiagScriptTester

data DiagScripter = DiagTest { script :: String }
      deriving (Show, Data, Typeable)

diagTest = mode $ DiagTest { script = def &= typDir & text "diagnoser script to run" }

  -- filterPath = def &= typDir & flag "P" & typ "DIR" & text "path of input file",
  -- filterRegex = def &= flag "R" & typ "REGEX" & text "regex to filter lines",
  -- inverse = def &= text "invert filtering (filter out everything that does NOT match)"

main ::  IO ()
main = withSocketsDo $ do 
  actions <- cmdArgs "DiagScripter 0.1.0, (c) Oliver Mueller 2010" modes
  execute actions

execute :: DiagScripter -> IO ()
execute (DiagTest s) = do
    print $ "running script " ++ s
    runTestScript s

modes = [diagTest]
