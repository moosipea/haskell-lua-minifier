function foo(bar)
	return bar * (1 << 4)
end

-- Do the jam
local baz = foo(42)
print(baz)
