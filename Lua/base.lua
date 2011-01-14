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
  --print("calling haskell to send message",Diag.tostring(a))
  local resp = send(msg2wireFormat(a))
	for i=2,string.len(resp) do
		--print(string.format("0x%x",string.byte(resp,i)))
	end
  return wireformat2msg(resp)
end
		
function execTests(tests)
	loop(1,tests)
end

function loop(n,tests)
	local results = {}
	results["passed"] = 0
	results["failed"] = 0
	for i = 1,n do
		for i,test in ipairs(tests) do
			local msg = test.send
			local resp = sendMsg(msg)
			if Diag.match(resp,test.expect) then
				results["passed"] = results["passed"] + 1
				print("success! for ",test.name)
			else
				results["failed"] = results["failed"] + 1
				print("FAILURE!!! for ",test.name)
			end
		end
		local p = results["passed"]
		local f = results["failed"]
		if f>0 then
			print(string.format("%d test were run, %d succeeded, %d failed",(p+f),p,f))
		else
			print(string.format("All tests passed (%d test were run)",p))
		end
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
--test()
