local UNIX, WINDOWS = false, true

os.execute("rm *.hi")
os.execute("rm *.o")
os.execute("rm nicompiler.exe")

function clear()
	if UNIX then
		os.execute("clear")
	elseif WINDOWS then
		os.execute("cls")
	end
end

clear()

local success = os.execute("lua invokebodygenerator.lua > invokebody.c") == 0
if not success then
	print("nicompiler preparation failed (invokebodygenerator.lua failed)")
	return -1
end

local success = os.execute("ghc -Wall -o nicompiler.exe main.hs") == 0
if success then
	clear()
	print("nicompiler built successfully")
	print("use `ni.lua` to run nickel scripts")
	return 0
else
	print("nicompiler build failed.")
	return -1
end