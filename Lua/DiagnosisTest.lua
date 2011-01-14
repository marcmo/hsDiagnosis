require "lunatest"
require "Diagnosis"
require "logging"

m1=Diag.new{0x22,0xF1}
m2=Diag.new{0x22,0xF1}
m3=Diag.new{0x31,0xF1}
m4=Diag.new{0x31,0xF1,0x0}
m5=Diag.new{0x31,0xF1,0x0,0x1,0x2,0x3,0x4,0x5}

local logger = logging.new(function(self, level, message)
                             print(level, message)
                             return true
                           end)
logger:setLevel (logging.WARN)

function test_equality()
	assert_true(m1==m2)
	assert_true(m1~=m3,"should NOT be equals")
end

function test_match_double_wildcard() assert_true(Diag.match(m1,{"*","*"})) end
function test_matching()
	assert_true(Diag.match(m1,{"*"}))
	assert_true(Diag.match(m1,{0x22,"*"}))
	assert_true(Diag.match(m1,{"*",0xF1}))
	assert_false(Diag.match(m4,{"*",0xF1}),"should not match (missing trailing byte)")
	assert_true(Diag.match(m5,{0x31,"*",0x1,"*",0x5}))
	assert_false(Diag.match(m5,{0x31,"*",0x1,"*",0x4}),"last byte not matching")
	assert_true(Diag.match(m5,{0x31,"*",0x1,"*"}))
end

function test_msg_length()
	assert_equal(Diag.length(m1), 2)
	assert_equal(Diag.length(m4), 3)
	assert_equal(Diag.length(Diag.new{0x22,0xF1,0x0}), 3)
end

test = DiagTest.new {
	name="TEST_DIAG_JOB",
	send=Diag.new{0x22,0xf1,0x90},
	expect=Diag.new{0xff,0x10,0x01,"*"},
	timeout=2000,
	source=0xf4, target=0x40 }

local function execTest2(t)
	print(t.name)
	print(string.format("sending %s to 0x%x",Diag.tostring(t.send),t.target))
	local answer = {0xff,0x10,0x01,0x02}
	print("matching:")
	print(Diag.match(answer,t.expect))
end
execTest2(test)
function string:split(sep)
	local sep, fields = sep or ":", {}
	local pattern = string.format("([^%s]+)", sep)
	self:gsub(pattern, function(c) fields[#fields+1] = c end)
	return fields
end

local sep = "\\"
local input = "b\241\144\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\253"
local rr = input:split(sep)
function inspectTable2(t)
  local _t = {}
  for _,v in pairs(t) do
    _t[#_t + 1] = v
  end
  return "{" .. table.concat(_t, ", ") .. "}"
end
print(inspectTable2(rr))
print("length was:",string.len(input))
local start
if string.byte(input) == 0x62 then
	start = 2
else
	start = 1
end
for i=start,string.len(input) do
	print(string.format("0x%x",string.byte(input,i)))
end

--lunatest.run()
print("logging:")
logger:log(logging.INFO, "sending email")
logger:log(logging.WARN, "sending email")
