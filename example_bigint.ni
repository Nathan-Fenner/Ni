var n : Integer = big 600851475143;

var ZERO : Integer = big 0;
var TWO : Integer = big 2;

func largestPrimeFactor (n : Integer) : Integer {
	if iEquals ZERO (iMod n (big 2)) {
		return largestPrimeFactor (iDivide n (big 2));
	}
	var d : Integer = big 3;
	while not ( iLess n (iMultiply d d) ) {
		if iEquals ZERO (iMod n d) {
			return largestPrimeFactor (iDivide n d);
		}
		d = iAdd d TWO;
	}
	return n;
}

func main! {
	iPrint (largestPrimeFactor n) !;
}