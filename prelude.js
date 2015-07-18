
var BANG = {/*bang!*/};

function CALL(fun, args) {
	if (!(args instanceof Array)) {
		throw args;
	}
	if (!fun) {
		if (args.length != 0) {
			throw args;
		}
		return fun;
	}
	if (fun.sofar === undefined) {
		// Create a partially-applied object.
		return CALL({ nargs: fun.nargs, fun:fun.fun, sofar: [] }, args);
	}
	if (fun.sofar.length >= fun.nargs) {
		// Overflow! Time to call!
		var result = fun.fun(fun.sofar);
		return CALL(result, args);
	}
	if (args.length === 0) {
		return fun;
	}
	var copy = { nargs: fun.nargs, fun: fun.fun, sofar: fun.sofar.slice(0) };
	copy.sofar.push( args[0] );
	return CALL(copy, args.slice(1));
}

function ADD(x, y) {
	return x + y;
}
function SUBTRACT(x, y) {
	return x - y;
}
function MULTIPLY(x, y) {
	return x * y;
}
function DIVIDE(x, y) {
	return (x / y)|0;
}
function MODULO(x, y) {
	return x % y;
}
function EQUAL(x, y) {
	return x == y;
}
function NOT_EQUAL(x, y) {
	return x != y;
}
function GREATER_OR_EQUAL(x, y) {
	return x >= y;
}
function LESS_OR_EQUAL(x, y) {
	return x <= y;
}
function GREATER(x, y) {
	return x > y;
}
function LESS(x, y) {
	return x < y;
}
function CONCAT(x, y) {
	return x + y;
}
function NEGATE(x) {
	return -x;
}
function print(args) {
	console.log(args[0]);
}


