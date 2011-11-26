module DiagnosticConfig
     (
      mkConf,
      conf,
      broadcastConf,
      femConfig,
      zgwConfig,
      msgTunnelConf,
      standardDiagTimeout,
      currentIp,
      msgTunnelIp,
      setIp,
      getIp
     ) 

where

import Com.DiagClient
import Data.IORef

debug_on = False

conf = femConfig ip_A
broadcastConf = femConfig "255.255.255.255"

ip_A = "10.40.39.22"
msgTunnelIp = "10.40.39.48"

fem = 0x40
zgw = 0x10
messageTunnel = 0x10

femConfig = mkConf fem
zgwConfig = mkConf zgw
msgTunnelConf = mkConf messageTunnel msgTunnelIp

standardDiagTimeout = 5000 :: Int -- ms
mkConf :: Word8 -> String -> DiagConfig
mkConf target ip = MkDiagConfig ip 6801 0xf4 target debug_on standardDiagTimeout

globalConfig ::  IO (IORef String)
globalConfig = newIORef "config"

setIp ip = do
   r <- globalConfig 
   modifyIORef r (const "new") 

getIp = do
  c <- globalConfig
  c'' <- readIORef c
  print c''

currentIp = let (MkDiagConfig ip _ _ _ _ _) = conf in
  ip
