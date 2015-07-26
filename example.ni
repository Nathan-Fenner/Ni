
func id <t> (x : t) : t {
	return x;
}

func first <a, b> (x : a) (y : b) : a {
	return x;
}

func succ (x : Int) : Int {
	return x + 1;
}

func main! {
	var inter : Int -> Int = id;
	var q : <a> a -> a = id;
	print (q 3)!;
}