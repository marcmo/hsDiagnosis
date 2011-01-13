print"loading Diagnosis"
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
	local result = true
	local parseAction = {
		["*"] = function (v,ppos) 
			print (string.format("in *, patPos = %d",patPos)) 
			if v == nextChar then
				print(string.format("in *, next char did match 0x%x",v))
				return true,ppos+2,"hex"
			else
				print(string.format("in *, wild card match for 0x%x",v))
				return true,ppos,"*"
			end
		end,
		["hex"] = function (v,ppos) 
			print (string.format("in hex, patPos = %d",patPos)) 
			if (v == pattern[ppos]) then
				print(string.format ("in hex, got match for 0x%x",v))
				return true,ppos+1,"hex"
			elseif (pattern[ppos]=="*") then
				print(string.format("in hex, was wildcard match for 0x%x",v))
				return true,ppos,"*"
			else
				--print(string.format("in hex, was nothing for 0x%x, pattern[%d] was %s",v,ppos,pattern[ppos]))
				return false
			end
			return ppos,"",1
		end
	}
	for i,v in ipairs(msg) do
		--print("checking pair for"..v.." and pat:"..pattern[patPos])
		result,patPos,state = parseAction[state](v,patPos)
		if not(result) then break end
		nextChar = pattern[patPos+1]
	end
	return result
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


