module Script.LoggingFramework
where
import Com.DiagClient
import DiagnosticConfig
import Data.Maybe(isJust,fromJust)
import Util.Encoding
import Util.Timer

data LoggingInstruction = DISABLEALL
                        | ENABLEALL
                        | SETLEVEL
                        | DISPLAYSETTINGS
                           deriving (Eq, Ord, Show, Read, Enum)

type UsedComponent = Component
-- type UsedComponent = ComponentBDC
data Component = COMMON
               | FZM
               | DEM
               | SYSTIMECLIENT
               | TPROUTER
               | TRANSPORT
               | DIAGNOSIS
               | SESSIONCONTROL
               | TAS
               | TEST
               | SCCLIENT
               | LIFECYCLE
               | CAN
               | EEPROM
               | DMCLIENT
               | EEE
               | CODING
               | LIN
               | FUSI
               | SWC
               | XCP
               | GLOBAL
                    deriving (Eq, Ord, Show, Read, Enum)
-- data ComponentBDC = CAN
--                   | CODING
--                   | COMMON
--                   | DEM
--                   | DIAGNOSIS
--                   | DMCLIENT
--                   | EEE
--                   | EEPROM
--                   | ETHERNET
--                   | FUSI
--                   | FZM
--                   | HTTP
--                   | LIFECYCLE
--                   | LIN
--                   | REMOTE
--                   | RPC
--                   | SCCLIENT
--                   | SELFDIAGNOSIS
--                   | SESSIONCONTROL
--                   | SWC
--                   | SYSTIMECLIENT
--                   | TAS
--                   | TCP
--                   | TEST
--                   | TPROUTER
--                   | TRANSPORT
--                   | UDP
--                   | XCP
--                   | ZEROCONF
--                   | GLOBAL
--                     deriving (Eq, Ord, Show, Read, Enum)

data LogLevel = NOLOGGING
              | DEBUG
              | INFO
              | WARN
              | ERROR
              | CRITICAL
                   deriving (Eq, Ord, Show, Read, Enum)


setLevel :: UsedComponent -> LogLevel -> IO [DiagnosisMessage]
setLevel comp level = sendData conf [0xbf,0x12,0x04,toWord SETLEVEL,toWord comp,toWord level]

disable = sendData conf [0xbf,0x12,0x04,toWord DISABLEALL] >> return ()

enable = sendData conf [0xbf,0x12,0x04,toWord ENABLEALL, toWord DEBUG] >>= print

showMapping = showMappingWithConf conf
showMappingWithConf c = sendData c [0xbf,0x12,0x04,toWord DISPLAYSETTINGS] >>=
                \xs -> if length xs == 0 then (print "no valid response")
                        else mapM_ (putStrLn . unlines . interpreteMapping . diagPayload) xs

interpreteMapping ::  [Word8] -> [String]
interpreteMapping bs = 
  let mapping = zip [0..] (drop 3 bs) 
      showLevel = show . (toEnum :: Int -> LogLevel) . word8ToInt
      showComponent = show . (toEnum :: Int -> UsedComponent) . word8ToInt
      display (pos,lev) = showComponent pos ++ " -> " ++ showLevel lev in
    map display mapping

