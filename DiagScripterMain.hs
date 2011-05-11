{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import Com.DiagClient(host)
import DiagnosticConfig(conf)
import Control.Monad (when)
import Network.Socket
import Test.DiagScriptTester(runTestScript)

data DiagScripter = DiagTest {
  script :: String,
  ip :: String,
  choice :: Bool
} deriving (Show, Data, Typeable)

diagTest = DiagTest {
                    script = def &= typDir &= help "diagnoser script to run",
                    ip = (host conf) &= help "ip address of ecu",
                    choice = enum [True &= help "say yes", False &= help "say no"]
                  } &= help "run diagnosis tests"

main ::  IO ()
main = withSocketsDo $ do 
  actions <- cmdArgs $ ((modes [diagTest])
                      &= summary "DiagScripter 0.1.0, (c) Oliver Mueller 2010-2011")
                      &= verbosity
  execute actions

execute :: DiagScripter -> IO ()
execute (DiagTest s ip c) = do
    print $ "running script " ++ s ++ " on ip " ++ ip ++ "choice: " ++ show c
    runTestScript s ip

