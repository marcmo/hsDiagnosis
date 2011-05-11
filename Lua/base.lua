print("loading base.lua")
require "Diagnosis"
require "loggingSupport"
local logger = logging.new(function(self, level, message)
                             print(level, message)
                             return true
                           end)
logger:setLevel (logging.WARN)

function wireformat2msg(m)
  assert(type(m)=="string","invalid message format")
  local a = {}
  local sm = split(m,"\,")
  for i,v in pairs(sm) do
  	table.insert(a,tonumber("0x"..v))
  end
  return Diag.new(a)
end
function msg2wireFormat(a)
  assert(type(a)=="table","invalid message format")
  local s=""
  for k,v in pairs(a) do
    if string.len(s) == 0 then
      s = s..string.format("%x",v)
    else
      s = s..","..string.format("%x",v)
    end
  end
  return s
end
local ipAddress = ""
function setIp(ip)
  ipAddress = ip
  print(ipAddress)
end

function sendMsg(source,target,timeout,a, debug)
  logger:log(logging.DEBUG, string.format("calling haskell to send message:%s",Diag.tostring(a)))
  local resp = send(ipAddress,source,target,timeout,debug,msg2wireFormat(a))
	for i=2,string.len(resp) do
		--print(string.format("0x%x",string.byte(resp,i)))
	end
  logger:log(logging.DEBUG, "response on lua side as string was:"..resp)
  return wireformat2msg(resp)
end
		
function execTests(tests)
	loop(1,tests)
end

function executeTest(test)
    local msg = test.send
    local resp = sendMsg(test.source, test.target, test.timeout, msg, false)
    return Diag.match(resp,test.expect)
end

function loop(n,tests)
	local results = {}
	results["passed"] = 0
	results["failed"] = 0
	for i = 1,n do
		for i,test in ipairs(tests) do
			if (executeTest(test)) then
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
function split(str, pat)
   local t = {}  -- NOTE: use {n = 0} in Lua-5.0
   local fpat = "(.-)" .. pat
   local last_end = 1
   local s, e, cap = str:find(fpat, 1)
   while s do
      if s ~= 1 or cap ~= "" then
	 table.insert(t,cap)
      end
      last_end = e+1
      s, e, cap = str:find(fpat, last_end)
   end
   if last_end <= #str then
      cap = str:sub(last_end)
      table.insert(t, cap)
   end
   return t
end

local function test()
  ms = Diag.new{0xA,0x0,0xC,0x1,0xFF,0x0,0x0,0x0,0x0}
  print("byte table",inspectTable(ms))
  print("as msg:",Diag.tostring(ms))
  wm = msg2wireFormat(ms)
  print("wiremsg:",wm)
  ms2 = wireformat2msg(wm)
  print("after wireformat2msg",inspectTable(ms2))
  print("as msg:",Diag.tostring(ms2))
  assert(ms==ms2,"message needs to be the same after conversion-round-trip")
end
--test()
