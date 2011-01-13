print"loading Diagnosis"
Diag = {} 
DiagTest = {}
DiagTest.prototype = { target = 0x10 }
DiagTest.mt={}

function Diag.new(t)
	print("new")
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

function Diag.tostring(msg)
	local s = ""
	local t = {}
  for i,e in ipairs(msg) do
    t[#t + 1] = e
    s = string.format("%s,0x%x",s,e)
  end
  print("Diag.tostring",s)
  --return "{" .. table.concat(t, ", ") .. "}"
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
	local nextChar = nil
	local state = "hex"
	local matching,not_matching,last_pattern_is_star = 0,1,2
	local result = matching
	local parseAction = {
		["*"] = function (v,ppos) 
			print (string.format("in *, patPos = %d",patPos)) 
			if ppos == #pattern then
				print("last pattern was star!")
				return last_pattern_is_star
			end
			if v == nextChar then
				print(string.format("in *, next char did match 0x%x",v))
				return matching,ppos+2,"hex"
			else
				print(string.format("in *, wild card match for 0x%x",v))
				return matching,ppos,"*"
			end
		end,
		["hex"] = function (v,ppos) 
			--print (string.format("in hex, ppos = %d, pattern:%s",ppos,pattern[ppos])) 
			--print ("type of v:",type(v))
			if (v == pattern[ppos]) then
				print(string.format ("in hex, got match for 0x%x",v))
				return matching,ppos+1,"hex"
			elseif (pattern[ppos]=="*") then
				print(string.format("in hex, was wildcard match for 0x%x",v))
				return matching,ppos,"*"
			else
				--print(string.format("in hex, was nothing for 0x%x, pattern[%d] was %s",v,ppos,pattern[ppos]))
				return not_matching
			end
			return ppos,"",1
		end
	}
	for i,v in ipairs(msg) do
		--print("checking pair for"..v.." and pat:"..pattern[patPos])
		result,patPos,state = parseAction[state](v,patPos)
		if not(result==matching) then break end
		nextChar = pattern[patPos+1]
	end
	return ((result==matching) or (result==last_pattern_is_star))
end
function Diag.length(msg)
	local s = 0
  for i,e in ipairs(msg) do
		print("i:",i,"e",e)
		s = s+1
	end
	return s
end

Diag.mt = {}    -- metatable for diagmessages

Diag.mt.__eq = Diag.equals


