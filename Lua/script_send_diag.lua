require "Lua/lunatest"

ip_address="10.40.39.13"

m=Diag.new{ 0x22,0xf1,0x90 }
mappingMsg=Diag.new{ 0xbf,0x12,0x4,0x3 }
readWriteBlockTest=Diag.new{ 0xbf,0x10,0x1,0x1 }

Diag.print(m)
respM=sendMsg(m)
print("sent! resp: " .. type(respM))
print("response as diagmessage:".. Diag.tostring(respM))


--assert_true(Diag.match(respM,{0x22,"*"}))
--resp=sendMsg(mappingMsg)
--print("sent! resp: " .. resp)
--resp=sendMsg(readWriteBlockTest)
--print("sent! resp: " .. resp)

--test = DiagTest.new { name="TEST_DIAG_JOB", send=Diag.new{0x22,0xf1,0x90}, expect=Diag.new{0xff,0x10,0x01}, timeout=2000, source=0xf4, target=0x40 }
--execTest(test)
--inspectTable(fromMsg(resp))
--print(sendMsg{ 0xbf,0x12,0x4,0x3 })
print("try to sleep...")
sleep(1)
print("woke up again!")

