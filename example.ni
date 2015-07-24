if 1 == 1 {
	var x : Int = 4;
	let {
		var m : Int = linCap 3;
		func linCap (n : Int) : Int {
			if n <= 100 {
				return n;
			}
			return 100 + m * (n - 100);
		}
	}
	print (linCap 5)!;
	print (linCap 105)!;
}