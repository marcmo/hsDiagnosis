module DiagnosticConfig
     (
      mkConf,
      conf,
      broadcastConf,
      femConfig,
      zgwConfig,
      messageTunnel,
      standardDiagTimeout,
      currentIp,
      msgTunnelIp,
      getIp
     )

where

import Com.DiagClient
import Data.IORef

debug_on = True

conf = zgwConfig ip_A
broadcastConf = femConfig "255.255.255.255"

ip_A = "172.31.95.242"
msgTunnelIp = "10.40.39.48"

fem = 0x40
zgw = 0x10
messageTunnel = 0x10 :: Word8

femConfig = mkConf fem
zgwConfig = mkConf zgw

standardDiagTimeout = 5000 :: Int -- ms
mkConf :: Word8 -> String -> DiagConfig
mkConf trg ip = MkDiagConfig ip 6801 0xf4 trg debug_on standardDiagTimeout

globalConfig ::  IO (IORef String)
globalConfig = newIORef "config"

getIp = do
  c <- globalConfig
  c'' <- readIORef c
  print c''

currentIp = let (MkDiagConfig ip _ _ _ _ _) = conf in
  ip

