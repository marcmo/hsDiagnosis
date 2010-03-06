{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import Com.DiagClient(host)
import DiagnosticConfig
import Control.Monad (when)
import Network.Socket
import Script.ErrorMemory
import Script.LoggingFramework
import Test.DiagScriptTester

data DiagScripter = DiagTest {
  script :: String,
  ip :: String,
  choice :: Bool
} deriving (Show, Data, Typeable)

diagTest = mode $ DiagTest {
                    script = def &= typDir & text "diagnoser script to run",
                    ip = (host conf) &= empty (host conf) & text "ip address of ecu",
                    choice = enum True [True &= text "say yes", False &= text "say no"]
                  } &= text "run diagnosis tests"

main ::  IO ()
main = withSocketsDo $ do 
  actions <- cmdArgs "DiagScripter 0.1.0, (c) Oliver Mueller 2010" modes
  execute actions

execute :: DiagScripter -> IO ()
execute (DiagTest s ip c) = do
    print $ "running script " ++ s ++ " on ip " ++ ip ++ "choice: " ++ show c
    runTestScript s

modes = [diagTest]
