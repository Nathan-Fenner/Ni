print("\t// abandon hope, all ye who enter here")
print("\t// TODO: eliminate this mess")
print("\tswitch (argc) {")
for i = 0, 127 do
	io.write("\tcase " .. i .. ":;\n\t\t")

	io.write("Value (*fn" .. i .. ")(")
	for j = 1, i do
		if j > 1 then
			io.write(", ")
		end
		io.write("Value")
	end
	io.write(") = (Value(*)(")
	for j = 1, i do
		if j > 1 then
			io.write(", ")
		end
		io.write("Value")
	end
	io.write("))function;\n")
	io.write("\t\treturn fn" .. i .. "(")
	for j = 1, i do
		if j > 1 then
			io.write(", ")
		end
		io.write("argv[" .. (j - 1) .. "]")
	end
	print(");")
end
print("\t}")
