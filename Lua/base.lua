print("loading base.lua")
require "Lua/Diagnosis"

function wireformat2msg(m)
  assert(type(m)=="string","invalid message format")
  local a = {}
	for i=1,string.len(m) do
		table.insert(a,string.byte(m,i))
	end
  return Diag.new(a)
end
function msg2wireFormat(a)
  assert(type(a)=="table","invalid message format")
  local s=""
  for k,v in pairs(a) do
    s=s..string.char(v)
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
	local msg = t.send
	local resp = sendMsg(msg)
	if Diag.match(resp,t.expect) then
		print("success! for ",t.name)
	else
		print("FAILURE!!! for ",t.name)
	end
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
function onWireRepresentation(m)
  local s = ""
	if string.len(m) > 0 then
		s = (string.format("0x%x",string.byte(m,1)))
		for i=2,string.len(m) do
			s = (string.format("%s,0x%x",s,string.byte(m,i)))
		end
	end
	return s
end
local function test()
  ms = Diag.new{0xA,0xB,0xC}
  print("byte table",inspectTable(ms))
  print("as msg:",Diag.tostring(ms))
  wm = msg2wireFormat(ms)
  print("wiremsg:",onWireRepresentation(wm))
  ms2 = wireformat2msg(wm)
  print("after wireformat2msg",inspectTable(ms2))
  print("as msg:",Diag.tostring(ms2))
  assert(ms==ms2,"message needs to be the same after conversion-round-trip")
end
test()
--dtest = DiagTest.new { name="WRITE_BLOCK_AND_READ_AGAIN_PLAIN", send=Diag.new{0xbf,0x10,0x01,0x0}, expect=Diag.new{0xff,0x10,0x01}, timeout=2000, source=0xf4, target=0x40 }
--print(inspectTest (dtest))
--DIAG [WRITE_BLOCK_AND_READ_AGAIN_PLAIN] SEND [bf,10,01,0] EXPECT [FF,10,01] TIMEOUT [2000] SOURCE [F4] TARGET [40]
