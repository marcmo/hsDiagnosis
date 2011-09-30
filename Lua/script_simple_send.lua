require("base")
require("Diagnosis")

local ip = "10.40.39.12"
setIp(ip)

local msg = Diag.new{0x22,0xF1,0x90}
local msg2 = Diag.new{0x22,0x25,0x08}
local timeout = 2000
local resp = sendMsg(0xF4, 0x10, timeout, msg, false)
print(resp)
local resp2 = sendMsg(0xF4, 0xdf, timeout, msg2, false)
wait(100)
sendMsgAsync(source, target, timeout, msg, false)



