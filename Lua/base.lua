
print("loading base.lua")
function fromMsg(m)
  assert(type(m)=="string","invalid message format")
  local i=1
  local a={}
  for c in m:gmatch"." do
    table.insert(a,string.byte(c))
  end
  return a
end
function toMsg(a)
  assert(type(a)=="table","invalid message format")
  local i=1
  local s=""
  for k,v in pairs(a) do
    s=s..string.char(v)
    i=i+1
  end
  return s
end
function sendMsg(a)
  print("calling send message")
  return send(toMsg(a))
end

function inspectTable(t)
  local l = {}
  for e in pairs(t) do
    l[#l + 1] = e
  end
  --print "{" .. table.concat(l, ", ") .. "}"
end
local function test()
  ms = {0xA,0xB,0xC}
  print(ms)
  m = toMsg(ms)
  print(m)
  ms2 = fromMsg(m)
  print(ms2)
end
