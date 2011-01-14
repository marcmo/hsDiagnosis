

function loop(n,t)
	for i = 1,n do
		print("loop "..i)
		for _,v in pairs(t) do
			v()
		end
	end
end

function test(x)
	return function() print("exec test for " .. x) end
end

loop(10, {
	test(3),
	test(4)
})


