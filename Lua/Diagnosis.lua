print"loading Diagnosis"
require "loggingSupport"

local logger = logging.new(function(self, level, message)
                             print(level, message)
                             return true
                           end)
logger:setLevel (logging.INFO)
Diag = {} 
DiagTest = {}
DiagTest.prototype = { target = 0x10 }
DiagTest.mt={}

function Diag.new(t)
	local msg = {}
	setmetatable(msg,Diag.mt)
	for i,v in ipairs(t) do
		msg[i] = v
	end
	return msg
end

function DiagTest.new(o)
	setmetatable(o,DiagTest.mt)
	return o
end
DiagTest.mt.__index = function (table,key)
	return DiagTest.prototype[key]
end
function DiagTest.tostring(t)
	return Diag.tostring(t.send)
end

function Diag.tostring(msg)
	local s = ""
  for i,e in ipairs(msg) do
    s = string.format("%s,0x%x",s,e)
  end
	return s
end

function Diag.print (s)
  print(Diag.tostring(s))
end

function Diag.equals(msg1,msg2)
	local res = true
  for i,v in ipairs(msg1) do
    res = res and msg2[i] == v
  end
	return res
end
function Diag.match(msg,pattern)
	local patPos = 1
  if Diag.length(msg) == 0 then
  	return pattern[patPos] == "*"
  end
	local nextChar = nil
	local state = "hex"
	local matching,not_matching,last_pattern_is_star = 0,1,2
	local result = matching
	local parseAction = {
		["*"] = function (v,ppos) 
			logger:log(logging.DEBUG, string.format("in *, patPos = %d",patPos))
			if ppos == #pattern then
				logger:log(logging.DEBUG, "last pattern was star!")
				return last_pattern_is_star
			end
			if v == nextChar then
				logger:log(logging.DEBUG, string.format("in *, next char did match 0x%x",v))
				return matching,ppos+2,"hex"
			else
				logger:log(logging.DEBUG, string.format("in *, wild card match for 0x%x",v))
				return matching,ppos,"*"
			end
		end,
		["hex"] = function (v,ppos) 
			--logger:log(logging.DEBUG, (string.format("in hex, ppos = %d, pattern:%s",ppos,pattern[ppos])) 
			--logger:log(logging.DEBUG, ("type of v:",type(v))
			if (v == pattern[ppos]) then
				logger:log(logging.DEBUG, string.format ("in hex, got match for 0x%x",v))
				return matching,ppos+1,"hex"
			elseif (pattern[ppos]=="*") then
				logger:log(logging.DEBUG, string.format("in hex, was wildcard match for 0x%x",v))
				return matching,ppos,"*"
			else
				--logger:log(logging.DEBUG, string.format("in hex, was nothing for 0x%x, pattern[%d] was %s",v,ppos,pattern[ppos]))
				return not_matching
			end
			return ppos,"",1
		end
	}
	for i,v in ipairs(msg) do
		--logger:log(logging.DEBUG, "checking pair for"..v.." and pat:"..pattern[patPos])
		result,patPos,state = parseAction[state](v,patPos)
		if not(result==matching) then break end
		nextChar = pattern[patPos+1]
	end
	local finalResult = (result==matching) or (result==last_pattern_is_star)
	if finalResult then
		--logger:log(logging.INFO, string.format("message %s matched the pattern %s", Diag.tostring(msg), Diag.tostring(pattern)))
  else
		logger:log(logging.INFO, "Ooops..! mesage did NOT match!")
  end
	return finalResult
end
function Diag.length(msg)
	local s = 0
  for i,e in ipairs(msg) do
		s = s+1
	end
	return s
end

Diag.mt = {}    -- metatable for diagmessages

Diag.mt.__eq = Diag.equals


