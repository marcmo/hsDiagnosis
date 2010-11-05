ip_address="10.40.39.39"
print("trying to send s.th")
resp=sendMsg{ 0x22,0xf1,0x90 }
inspectTable(fromMsg(resp))
--print(sendMsg{ 0xbf,0x12,0x4,0x3 })
--print("try to sleep...")
--sleep(1)
--print("woke up!")
