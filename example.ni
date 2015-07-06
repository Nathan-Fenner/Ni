

func fac (n :Int) : Int {
	var i : Int = 1
	var s : Int = 1
	while i <= n {
		s *= i
	}
	return s
}

func main ! : Void {
	print (fac 5)!
}
