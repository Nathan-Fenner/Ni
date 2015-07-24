# *Ni*ckel
The *Ni*ckel programming language.

Nickel is a strongly-typed, semifunctional imperative language with (sometimes!) non-strict semantics.

It's currently transpiled to JavaScript, with plans for a similar transpilation to C.

The compiler is written in Haskell, with no other external dependencies.

## Examples

### Hello, world!
```
putStr "Hello, world!" !;
```
### Simple math
```
var x : Int = 5;
var y : Int = 7;
print x!;
print y!;
print (x + y)!;
```
### Functions
```
func square (x : Int) : Int {
	return x*x;
}
print (square 4)!;
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
print (factorial 5)!;
```
### Higher-Order Functions
```
func twice (f : ! -> Void) ! {
	f!;
	f!;
}
twice (print 4)!;
```
### While-Loop
```
var count = 10;
var i = 1;
while i <= count {
	print i!;
	i = i + 1;
}
```
### Implicit Parameters
```
var count = 5;
func many (f : ! -> Void) ! {
	var i = 1;
	while i <= count {
		f!;
	}
}
many (print 7)!;
count = 2;
many (print 3)!;
```
Nickel implements "implicit parameters" rather than closures for functions.
This means that changing variables after a function definition won't have any effect on that function's behavior.

## Planned, but Non-Yet-Implemented Features (aka TODO)

* Compound datastructures
* Defining your own types
* Parametric polymorphism
* Type inference
* A standard library
* A foreign function interface
* Objects and Methods
* Concurrency features
* Better error messages

## Why the name?

Being named after a mineral suggests resilience and malleability.
Being named after a coin suggests being cheap to use - (I hope) you can avoid incurring lots of technical debt.
