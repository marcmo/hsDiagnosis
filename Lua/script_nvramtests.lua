require("base")

--local ip = "localhost"
local ip = "10.40.39.12"
setIp(ip)

require("nvramtests")
runNvramTests(10);

