package.path=package.path..';./?/*.lua'
print(package.path)

ys = { ONE = 1, TWO = 2}
xs = {1,2,3,4}
print(xs)
--for k,v in pairs(ys) do
--  print(k.." was "..v)
--end
function copy(t)
  local res = {}
  local i = 1
  for k,v in pairs(t) do
  	res[i] = v
  	i = i + 1
  end
  return res
end

c = copy(ys)

for i,v in ipairs(c) do
	print ("=---------------")
  print(i.." was "..v)
end

