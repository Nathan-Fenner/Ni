let {

func twice (n : Int) : Int {
	return n * 2;
}

func say (n : Int) ! {
	print n!;
	print n!;
	print n!;
}

func main {
	var x : Int = 1;
	while x < 20 {
		say (twice x)!;
		x = x + 1;
	}
}

}