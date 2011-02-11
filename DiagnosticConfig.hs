module DiagnosticConfig

where

import Com.DiagClient

debug_on = False

conf = femConfig ip_E
zgwConf = zgwConfig ip_A

ip_A = "10.40.39.21"
ip_B = "10.40.39.33"
ip_C = "10.40.39.5"
ip_D = "10.40.39.44"
ip_E = "10.40.39.7"
ip_F = "10.40.39.49"
msgTunnelIp = "10.40.39.48"

fem = 0x40
zgw = 0x10
messageTunnel = 0x10

femConfig = mkConf fem
zgwConfig = mkConf zgw
msgTunnelConf = mkConf messageTunnel msgTunnelIp

mkConf :: Word8 -> String -> DiagConfig
mkConf target ip = MkDiagConfig ip 6801 0xf4 target debug_on


