
struct Pair t {
	x : t;
	y : t;
}

func add (a : Pair Int) (b : Pair Int) : Pair Int {
	return {Pair Int | x = a.x + b.x, y = a.y + b.y};
}

func printPair (p : Pair Int)! {
	putStr ("(" ++ show p.x ++ ", " ++ show p.y ++ ")")!;
}

func main! {
	printPair (add {Pair Int | x = 3, y = 7} {Pair Int | x = 2, y = 1})!;
}

