# *Ni*ckel
The *Ni*ckel programming language.

Nickel is a strongly-typed, semifunctional imperative language with (sometimes!) non-strict semantics.

It's currently transpiled to JavaScript, with plans for a similar transpilation to C.

The compiler is written in Haskell, with no other external dependencies. I plan on bootstrapping it, eventually.

## Examples

### Hello, world!
```
func main! {
	putStr "Hello, world."!;
}
```
### Simple math
```
func main! { 
	var x : Int = 5;
	var y : Int = 7;
	print x!;
	print y!;
	print (x + y)!;
}
```
### Functions
```
func square (x : Int) : Int {
	return x*x;
}
func main! {
	print (square 4)!;	
}
```
### Partial Application with Currying
```
func multiplier (x : Int) (y : Int) : Int {
	return x * y;
}
var doubler : Int -> Int = multiplier 2;
var tripler : Int -> Int = multiplier 3;
func main! {
	print (doubler 5)!;
	print (tripler 5)!;
}
```
### Recursive Functions
```
func factorial (n : Int) : Int {
	if n == 0 {
		return 1;
	} else {
		return n * factorial (n-1);
	}
}
func main! {
	print (factorial 5)!;	
}
```
### Higher-Order Functions
```
func twice (f : ! -> Void) ! {
	f!;
	f!;
}
func main! {
	twice (print 4)!;
}
```
### While-Loop
```
func main! {
	var count : Int = 10;
	var i : Int = 1;
	while i <= count {
		print i!;
		i = i + 1;
	}
}
```
### Implicit Parameters
```
func main! {
	var count : Int = 5;
	func many (f : ! -> Void) ! {
		var i : Int = 1;
		while i <= count {
			f!;
			i = i + 1;
		}
	}
	many (print 7)!;
	count = 2;
	many (print 3)!;
}
```
### Structs (a WIP)
```
struct Pair {
	x : Int;
	y : Int;
}

func add (a : Pair) (b : Pair) : Pair {
	return Pair{x = a.x + b.x, y = a.y + b.y};
}

func printPair (p : Pair)! {
	putStr ("(" ++ show p.x ++ ", " ++ show p.y ++ ")")!;
}

func main! {
	printPair (add Pair{x = 3, y = 7} Pair{x = 2, y = 1})!;
}
```
Nickel implements "implicit parameters" rather than closures for functions.
This means that changing variables after a function definition won't have any effect on that function's behavior.

## Planned, but Non-Yet-Implemented Features (aka TODO)

* Parametric polymorphism
* Type inference
* A standard library
* A foreign function interface
* Objects and Methods
* Concurrency features
* Better error messages
