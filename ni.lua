local UNIX, WINDOWS = false, true

function clear()
	if UNIX then
		os.execute("clear")
	elseif WINDOWS then
		os.execute("cls")
	end
end

-- Builds a .ni file
if #arg ~= 1 then
	print("usage")
	print("\tlua.ni [nickel file]")
	return os.exit(-2)
end

local file = arg[1]
print("[ compilng with nickel . . .   ]")
local success = os.execute("nicompiler " .. file) == 0

if success then
	clear()
	print("[ nickel compiled successfully ]")
	print("[ compiling with gcc . . .     ]")
	-- The Nickel Compiler was happy with your code :)
	-- Will GCC OK the output?
	local gccSuccess = os.execute("gcc -O0 out.c -o niout.exe") == 0
	if gccSuccess then
		clear()
		print("[ nickel compiled successfully ]")
		print("[ gcc compiled successfully    ]")
		print("[ running                      ]")
		if WINDOWS then
			os.execute("niout")
		elseif UNIX then
			os.execute("./niout.exe")
		end
		return os.exit(0)
	end
end

return os.exit(-1)