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
import Util.Encoding
import Data.Word
import LuaTester(executeLuaScript)
import Diagnoser.DiagScriptTester(runTestScript)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Token
import Control.Applicative

data DiagTool = Diagsend { ip :: String, diagId :: String, message :: String }
              | ReadDtc { ip :: String, dtcKind :: Int }
              | Logging { logIp :: String, enableLogging :: Bool, showLogging :: Bool }
              | DiagTest { ip :: String, script :: String }
              | LuaTest { script :: String }
                deriving (Show, Data, Typeable)
defaultIp = "127.0.0.1"
diagSend = Diagsend {ip = defaultIp &= help "ip address",
                            diagId = "10" &= help "diagnosis id",
                            message = "[0x22,0xF1,0x90]" &= help "diagnostic message to be sent"}
dtc = ReadDtc { ip = defaultIp &= help "ip address",
                dtcKind = 1 &= name "k" &= help "[primary = 1, secondary = 2]" } &= help "read DTCs in ecu"

logging = Logging { logIp = defaultIp &= name "i" &= help "ip address",
                           enableLogging = def &= help "enable logging",
                           showLogging = def &= help "show mapping"
                         } &= help "change logging settings"

diagTest = DiagTest { ip = defaultIp &= help "ip address",
                      script = def &= help "diagnoser script to run" }
luaTest = LuaTest { script = def &= help "lua script to run" }

parseTesterId ::  String -> Either ParseError Word8
parseTesterId = parse hexnumber "(unknown)"
parseDiagMsg ::  String -> Either ParseError [Word8]
parseDiagMsg = parse hexlist "(unknown)"
hexlist :: CharParser () [Word8]
hexlist = between (char '[') (char ']') (hexnumber `sepBy` char ',')
hexnumber = fst . head . readHex <$> (skipMany (string "0x") *> many1 hexDigit)

main ::  IO ()
main = withSocketsDo $ do
  actions <- cmdArgs $ (modes [diagSend,dtc,logging,diagTest,luaTest]
                      &= summary "DiagnosisTool 0.3.0, (c) Oliver Mueller 2010-2011")
                      &= verbosity
  execute actions

execute :: DiagTool -> IO ()
execute (ReadDtc ip x)
  | x == 1 = readPrimaryErrorMemory c
  | x == 2 = readSecondaryErrorMemory c
    where c = femConfig ip
execute (Logging logIp e m) = do
  when e enable
  when m $ showMappingWithConf conf
execute (DiagTest ip s) = do
--    print $ "running script " ++ s
--    runTestScript s ip
  return ()
execute (Diagsend ip diagid m) =
  case parseTesterId diagid of
    (Right tid) ->  do  let config = mkConf tid ip
                        case parseDiagMsg m of
                          (Right msg) -> sendData config msg >> return ()
                          _ -> print "diag message format not correct (use s.th. like 0x22,0xF1)" >> return ()
    _ -> return ()

