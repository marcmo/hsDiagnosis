import DiagClient
import DiagnosticConfig
import Data.Maybe(isJust,fromJust)
import Util
import Timer

data LoggingInstruction = DISABLEALL
                        | ENABLEALL
                        | SETLEVEL
                        | DISPLAYSETTINGS
                           deriving (Eq, Ord, Show, Read, Enum)

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
               | GLOBAL
                    deriving (Eq, Ord, Show, Read, Enum)

data LogLevel = NOLOGGING
              | DEBUG
              | INFO
              | WARN
              | ERROR
              | CRITICAL
                   deriving (Eq, Ord, Show, Read, Enum)

sourceAddr = string2hex source
targetAddr = string2hex target

getVin = sendData [0x22,0xF1,0x90]

setLevel comp level = sendData [0xbf,0x12,0x04,toWord SETLEVEL,toWord comp,toWord level]

disable = sendData [0xbf,0x12,0x04,toWord DISABLEALL] >> return ()

enable = sendData [0xbf,0x12,0x04,toWord ENABLEALL] >> return ()

showMapping = sendData [0xbf,0x12,0x04,0x03] >>=
                maybe (print "no valid response")
                  (putStrLn . unlines . interpreteMapping . diagPayload)

interpreteMapping ::  [Word8] -> [String]
interpreteMapping bs = 
  let mapping = zip [0..] (drop 3 bs) 
      showLevel = show . (toEnum :: Int -> LogLevel) . word8ToInt
      showComponent = show . (toEnum :: Int -> Component) . word8ToInt
      display (pos,lev) = showComponent pos ++ " -> " ++ showLevel lev in
    map display mapping

toWord = int2Word8 . fromEnum
