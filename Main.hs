{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import Com.DiagClient(sendData)
import DiagnosticConfig
import Control.Monad (when)
import Network.Socket
import Script.ErrorMemory
import Script.LoggingFramework
import Numeric(showHex,readHex)
import Util
import Data.Word
import Test.DiagScriptTester(runTestScript)
import ParserUtil
import Text.Parsec.Token

data DiagTool = Diagsend { ip :: String, diagId :: String, message :: String }
              | ReadDtc { dtcKind :: Int }
              | Logging { logIp :: String, enableLogging :: Bool, showLogging :: Bool }
              | DiagTest { script :: String }
                deriving (Show, Data, Typeable)

diagSend = mode $ Diagsend {ip = "10.40.39.68" &= text "ip address",
                            diagId = "10" &= text "diagnosis id",
                            message = "[0x22,0xF1,0x90]" &= text "diagnostic message to be sent"}
dtc = mode $ ReadDtc { dtcKind = 1} &= text "read DTCs in ecu (primary = 1, secondary = 2)"
logging = mode $ Logging { logIp = "10.40.39.68" &= text "ip address",
                           enableLogging = def &= text "enable logging",
                           showLogging = def &= text "show mapping"
                         } &= text "change logging settings"

diagTest = mode $ DiagTest { script = def &= text "diagnoser script to run" }

parseTesterId ::  String -> Either ParseError Word8
parseTesterId = parse hexnumber "(unknown)"
parseDiagMsg ::  String -> Either ParseError [Word8]
parseDiagMsg input = parse hexlist "(unknown)" input
hexlist :: CharParser () [Word8]
hexlist = between (char '[') (char ']') (hexnumber `sepBy` char ',')
hexnumber = fst . head . readHex <$> (skipMany (string "0x") *> many1 (hexDigit))

parseTestA ::  String -> Either ParseError Char
parseTestA = parse test "(unknown)"
test :: CharParser () Char
test = skipMany (string "0x") *> hexDigit

main ::  IO ()
main = withSocketsDo $ do 
  actions <- cmdArgs "DiagnosisTool 0.1.0, (c) Oliver Mueller 2010" modes
  execute actions

execute :: DiagTool -> IO ()
execute (Diagsend ip diagid m) = do
  putStrLn $ "send: " ++ show m ++ " to " ++ ip ++ " (diag-addr.:" ++ diagid ++ ")"
  case parseTesterId diagid of
    (Right tid) ->  do  let config = mkConf (drop 2 $ showAsHex tid) ip
                        case parseDiagMsg m of
                          (Right msg) -> sendData config msg >> return ()
                          _ -> print "diag message format not correct (use s.th. like 0x22,0xF1)" >> return ()
    _ -> return ()
execute (ReadDtc x)
  | x == 1 = readPrimaryErrorMemory
  | x == 2 = readSecondaryErrorMemory
execute (Logging logIp e m) = do
  let config = mkConf (drop 2 $ showAsHex tid) ip
  when e enable
  when m showMappingWithConf
execute (DiagTest s) = do
    print $ "running script " ++ s
    runTestScript s

modes = [diagSend,dtc,logging,diagTest]
