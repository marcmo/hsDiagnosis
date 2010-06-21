module DiagnosticConfig

where

import Com.DiagClient

debug_on = False

conf = femConfig ip_E
zgwConf = zgwConfig ip_A

ip_A = "10.40.39.26"
ip_B = "10.40.39.33"
ip_C = "10.40.39.5"
ip_D = "10.40.39.44"
ip_E = "10.40.39.27"
msgTunnelIp = "10.40.39.68"

fem = "40"
zgw = "10"
messageTunnel = "12"

femConfig = mkConf fem
zgwConfig = mkConf zgw
msgTunnelConf = mkConf messageTunnel msgTunnelIp

mkConf :: String -> String -> DiagConfig
mkConf target ip = MkDiagConfig ip 6801 "f4" target debug_on


