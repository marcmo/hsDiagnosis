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

data DiagTool = Diagsend { message :: [Int]}
              | ReadDtc { dtcKind :: Int }
              | Logging { enableLogging :: Bool, showLogging :: Bool }
              | DiagTest { script :: String }
                deriving (Show, Data, Typeable)

diagSend = mode $ Diagsend {message = def &= text "diagnostic message to be sent"}
dtc = mode $ ReadDtc { dtcKind = 1} &= text "read DTCs in ecu (primary = 1, secondary = 2)"
logging = mode $ Logging { enableLogging = def &= text "enable logging",
                           showLogging = def &= text "show mapping"
                         } &= text "change logging settings"

diagTest = mode $ DiagTest { script = def &= text "diagnoser script to run" }

  -- filterPath = def &= typDir & flag "P" & typ "DIR" & text "path of input file",
  -- filterRegex = def &= flag "R" & typ "REGEX" & text "regex to filter lines",
  -- inverse = def &= text "invert filtering (filter out everything that does NOT match)"

main ::  IO ()
main = withSocketsDo $ do 
  actions <- cmdArgs "DiagnosisTool 0.1.0, (c) Oliver Mueller 2010" modes
  execute actions

execute :: DiagTool -> IO ()
execute (Diagsend m) = do
  putStrLn $ "send: " ++ show m
  sendData conf [0x22,0xF1,0x90]
  return ()
execute (ReadDtc x)
  | x == 1 = readPrimaryErrorMemory
  | x == 2 = readSecondaryErrorMemory
execute (Logging e m) = do
  when e enable
  when m showMapping
execute (DiagTest s) = do
    print $ "running script " ++ s
    runTestScript s

modes = [diagSend,dtc,logging,diagTest]
