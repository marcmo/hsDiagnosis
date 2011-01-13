print("loading base.lua")
require "Lua/Diagnosis"

function wireformat2msg(m)
  assert(type(m)=="string","invalid message format")
  print("wireformat2msg_start",m)
  print("wireformat2msg_start,length(m)",string.len(m))
	print(string.format("0x%x",string.byte(m,2)))
	print(string.format("0x%x",string.byte(m,3)))
	print(string.format("0x%x",string.byte(m,20)))
  local a = {}
  local s = ""
	local start
	if string.byte(m) == 0x62 then
		start = 2
	else
		start = 1
	end
	for i=start,string.len(m) do
		table.insert(a,string.byte(m,i))
		s = (string.format("%s---0x%x",s,string.byte(m,i)))
	end
	print("wireformat2msg:",s)
  return Diag.new(a)
end
function msg2wireFormat(a)
  assert(type(a)=="table","invalid message format")
  local s=""
  for k,v in pairs(a) do
    s=s..string.char(v)
		print(string.format("inside msg2wireFormat,a[%d]=0x%x,char -->%s<--",k,v,(string.char(v))))
  end
  return s
end
function sendMsg(a)
  print("calling haskell to send message",Diag.tostring(a))
  local resp = send(msg2wireFormat(a))
  print("got response",resp)
	for i=2,string.len(resp) do
		--print(string.format("0x%x",string.byte(resp,i)))
	end
  return wireformat2msg(resp)
end
function execTest(t)
	print("execute Test:",t.name)
	msg=t.send
  print("send",inspectTable(msg))
	resp=sendMsg(send)
	assert(resp==t.expect,"response should be as expected")
end

function inspectTable(t)
  local _t = {}
  for _,v in pairs(t) do
    _t[#_t + 1] = v
  end
  return "{" .. table.concat(_t, ", ") .. "}"
end
function inspectTest(t)
	print("name",t.name)
  print("send",inspectTable(t.send))
  print("expect",inspectTable(t.expect))
  print(t.timeout)
  print(t.source)
  print(t.target)
end
local function test()
  ms = Diag.new{0xA,0xB,0xC}
  print("byte table",inspectTable(ms))
  print("as msg:",Diag.tostring(ms))
  wm = msg2wireFormat(ms)
  print("after toMsg:",wm)
  ms2 = wireformat2msg(wm)
  print("after wireformat2msg",inspectTable(ms2))
  print("as msg:",Diag.tostring(ms2))
  assert(ms==ms2,"message needs to be the same after conversion-round-trip")
end
test()
--dtest = DiagTest.new { name="WRITE_BLOCK_AND_READ_AGAIN_PLAIN", send=Diag.new{0xbf,0x10,0x01,0x0}, expect=Diag.new{0xff,0x10,0x01}, timeout=2000, source=0xf4, target=0x40 }
--print(inspectTest (dtest))
--DIAG [WRITE_BLOCK_AND_READ_AGAIN_PLAIN] SEND [bf,10,01,0] EXPECT [FF,10,01] TIMEOUT [2000] SOURCE [F4] TARGET [40]
