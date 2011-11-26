local lpeg = require "lpeg"

-- matches a word followed by end-of-string
p = lpeg.R"az"^1 * -1

--print(p:match("hello"))        --> 6
--print(lpeg.match(p, "hello"))  --> 6
--print(p:match("1 hello"))      --> nil

digits = lpeg.R ("09")
lowerHex = lpeg.R("ad")
upperHex = lpeg.R("AD")
hexChar = lowerHex + upperHex + digits


hexValue = lpeg.P ("0x") * hexChar * hexChar^-1
hexValueCapture = lpeg.C (hexValue)
comma_hexvals		 = hexValue * ("," * hexValue)^0
comma_hexvalues = hexValueCapture * ("," * hexValueCapture)^0

print (lpeg.match (hexValue, "0xa3"))
print (lpeg.match (hexValueCapture, "a3")) 
print (lpeg.match (comma_hexvals, "0xa3,0xAC,0xB5,0xA"))
--matchpatt = "0xa3,*,0xA"
matchpatt = lpeg.P("0xa3") 
print ("does it match?", lpeg.match(matchpatt,"0xa3"))
matchpatt = lpeg.P("0xa3") * (",") 
print ("does it match?", lpeg.match(matchpatt,"0xa3,"))
matchpatt = lpeg.P("0xa3") * (",") * comma_hexvals^0 
print ("does it match?", lpeg.match(matchpatt,"0xa3,0xAC,0xB5,0xA"))
matchpatt = lpeg.P("0xa3") * (",") * comma_hexvals^0 * (",") * lpeg.P("0xA") 
print ("does it match?", lpeg.match(matchpatt,"0xa3,0xAC,0xB5,0xA"))

result = lpeg.match (lpeg.Ct (comma_hexvalues), "0xa3,0xaa,0xB5,0xA")

if result then
	print "ok"
  for i,e in ipairs(result) do
  	print(i,e)
  end
else
  print "no match"
end -- if

function matchMsg(m)
	local res = {} 
	for i,e in ipairs(m) do
		res[i] = string.format("0x%x",e)
	end
	return table.concat(res, ",")
end
m={0x3f,0x4f}
print(matchMsg(m))

function mapIt(msg)
  for i,e in ipairs(msg) do
  	print(i,e)
  end
end


function testMatch(m)
	local s = ""
  for i,e in ipairs(m) do
  	s = s..'<'..e..'>'
  	print(s)
  end
	print (string.match(s,'<..>'))
end

--m={11,92,83}
--testMatch(m)


