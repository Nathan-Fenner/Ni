# *Ni*ckel
The Nickel programming language.

Nickel is a strongly-typed, semifunctional imperative language with (sometimes!) non-strict semantics.

It's currently transpiled to JavaScript, with plans for a similar transpilation to C.

The compiler is written in Haskell, with no other external dependencies. I plan on bootstrapping it, eventually.

## Design Goals
Nickel is intended to be extremely easy to read, reason about, and refactor.

In particular, Nickel eliminates references and uncontrolled side effects so that code can be reasoned about in an entirely local manner. Programs in Nickel should be designed to transform data, rather than mutate it.

## Examples

### Hello, world!
```go
func main! {
	putStr "Hello, world"!;
}
```
### Simple math
```go
func main! { 
	var x : Int = 5;
	var y : Int = 7;
	print x!;
	print y!;
	print (x + y)!;
}
```
### Functions
```go
func square (x : Int) : Int {
	return x*x;
}
func main! {
	print (square 4)!;	
}
```
### Partial Application with Currying
```go
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
```go
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
```go
func twice (f : ! -> Void)! {
	f!;
	f!;
}
func main! {
	twice (print 4)!;
}
```
### While-Loop
```go
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
```go
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
Although Nickel doesn't allow closures, when a function is defined, it may refer to any variable in scope as legal values. These are passed as implicit parameters to the function at its time of creation- if they change afterward, the function will not be affected.

### Compound Data Types
```go
struct Point {
	x : Int;
	y : Int;
}

func add (a : Point) (b : Point) : Point {
	return {Point | x = a.x + b.x, y = a.y + b.y};
}

func printPoint (p : Point)! {
	var str : String = "(" ++ show p.x ++ ", " ++ show p.y ++ ")";
	putStr str!;
}

func main! {
	printPoint (add {Point | x = 3, y = 7} {Point | x = 2, y = 1})!;
}
```
"Structs" are the basic way to store compound data. The struct instantiation syntax is a little unusual, but is concise and unambiguous.

Struct literal fields are always keyed, and no field is optional.

### Generic Data Types
```go
struct Pair <t> {
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
```
### Polymorphism
```go
func id <a> (x : a) : a {
	return x;
}
func first <a, b> (x : a) (y : b) : a {
	return x;
}
func second <a, b> (x : a) (y : b) : b {
	return y;
}
func succ (n : Int) : Int {
	return n + 1;
}

func main! {
	print (id 5)!;
	print (succ 5)!;
	print (first 2 5)!;
	var myID : <t> t -> t = id;
	var myFun : Int -> Int = id;
	print (myID 3)!;
	print (myFun 3)!;
}
```
Structs in Nickel can be made generic. They allow any number of generic parameters.

## Planned Features

* Non-concrete generics
* Type inference
* A larger standard library
* A foreign function interface
* Objects and Methods
* Concurrency features
* Better error messages
* Do-notation
