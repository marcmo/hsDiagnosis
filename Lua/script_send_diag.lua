require("base")

local ip = "10.40.39.19"
setIp(ip)
--setIp("localhost")

require("nvramtests")
runNvramTests(2);

--local msg=Diag.new{ 0x22,0x20,0x0 }
--local resp = sendMsg(0xf4, 0x40, 2000, msg)
--print("response was: " .. Diag.tostring(resp))


showPrimaryDtcs(ip,0xf4, 0x40)
showSecondaryDtcs(ip,0xf4, 0x40)
print("here")
logger:log(logging.WARN, "try to wait...")
wait(200)
print("woke up again!")
wait(200)
print("up")
wait(200)
print("again!")

