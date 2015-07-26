struct Pair t {
	left : t;
	right : t;
}

func add (p : Pair Int) : Int {
	return p.left + p.right;
}

func multiply (p : Pair Int) : Int {
	return p.left * p.right;
}

func concat (p : Pair String) : String {
	return p.left ++ ", " ++ p.right;
}

func main! {
	var myPair : Pair Int = {Pair Int | left = 3, right = 4 };
	var sum : Int = add myPair;
	var product : Int = multiply myPair;
	var both : Pair String = {Pair String | left = show sum, right = show product };
	putStr (concat both)!;
}