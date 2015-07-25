
struct Pair {
	x : Int;
	y : Int;
}

func add (a : Pair) (b : Pair) : Pair {
	return {Pair | x = a.x + b.x, y = a.y + b.y};
}

func printPair (p : Pair)! {
	putStr ("(" ++ show p.x ++ ", " ++ show p.y ++ ")")!;
}

func main! {
	printPair (add {Pair | x = 3, y = 7} {Pair | x = 2, y = 1})!;
}

