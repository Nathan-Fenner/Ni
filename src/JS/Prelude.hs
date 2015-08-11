
{-# LANGUAGE QuasiQuotes #-}

module JS.Prelude where

import Quotes

-- The prelude stores the little "runtime" we use to evaluate programs.

preludeSource :: String
preludeSource = [lit|
// -----------------------------------------------

// This file has been generated with Nickel, a toy programming language that combines functional and imperative features.
// See its project page here:
// https://github.com/Nathan-Fenner/Ni

// -----------------------------------------------
"use strict";
// -----------------------------------------------

// Begin prelude

var $Bang = {type: "bang"};
var $Unit = {type: "unit"};

function $Partial(fun, capacity, args) {
	if (typeof capacity != typeof 1) {
		throw "$Partial requires a capacity";
	}
	if (! (args instanceof Array)) {
		throw "$Partial requires args";
	}
	return {type:"partial", fun: fun, capacity: capacity, args: args};
}
function $Remember(thing) {
	return {type: "remember", thing: thing};
}
function $Call(fun, args) {
	return {type: "call", fun: fun, args: args};
}
function $Constructor(type, fields) {
	return {type:"constructor", name: type, fields: fields};
}
function $Dot(value, field) {
	return {type:"dot", value:value, field:field};
}
function $Force(e) {
	if (typeof e === "function") {
		throw { message: "tried to a force a function", fun:e };
	}
	if (typeof e === "number"
		|| typeof e === "string"
		|| typeof e === "boolean"
		|| e.type === "bang"
		|| e.type === "unit"
		|| e.type === "constructor") {
		return e; // these are properly atomic values
	}
	if (e instanceof BigInt) {
		return e; // atomic library value
	}
	if (e.type === "partial") {
		if (e.args.length < e.capacity) {
			return e;
		} else {
			if (e.args.length > e.capacity) {
				throw { message: "e.capacity < e.args.length", e: e };
			}
			// An invokation is required
			return $Force( e.fun.apply(undefined, e.args.map($Remember)) );
		}
	}
	if (e.type === "remember") {
		var value = $Force(e.thing);
		e.type = "stored";
		e.thing = value;
		return value;
	}
	if (e.type === "stored") {
		return e.thing;
	}
	if (e.type === "dot") {
		var value = $Force(e.value);
		if (value.type !== "constructor") {
			throw { message: "expected dot to be performed only on constructor", e: e, value: value };
		}
		return $Force(value.fields[e.field]);
	}
	if (e.type === "call") {
		// We have to force its function, then apply arguments one-by-one.
		if (e.args.length == 0) {
			// It's just its function
			return $Force(e.fun);
		}
		var partial = $Force(e.fun);
		if (partial.type !== "partial") {
			throw {message: "attempted to call a non-partial object", partial: partial, e: e};
		}
		if (partial.args.length >= partial.capacity) {
			throw {message: "received a supposedly-forced partial at capacity", partial: partial};
		}
		if (typeof partial.fun !== "function") {
			throw {message: "received a supposedly-forced partial without a proper function", partial: partial};
		}
		if (partial.args.length + e.args.length < partial.capacity) {
			return $Partial(partial.fun, partial.capacity, partial.args.concat(e.args));
		}
		var quantity = partial.capacity - partial.args.length;
		var newArgs = e.args.slice(0, quantity);
		var extraArgs = e.args.slice(quantity);
		var newFun = $Force(partial.fun.apply(undefined, partial.args.concat(newArgs)));
		return $Force($Call(newFun, extraArgs));
	}
	throw { message: "unfamiliar value to force", value: e};
}

function $MakeOperator(f) {
	return $Partial(function(x, y) {
		var X = $Force(x);
		var Y = $Force(y);
		return f(X, Y);
	}, 2, []);
}

var $Operator = {
	"+": $MakeOperator(function(x, y) { return (x + y)|0; }),
	"-": $MakeOperator(function(x, y) { return (x - y)|0; }),
	"*": $MakeOperator(function(x, y) { return (x * y)|0; }),
	"/": $MakeOperator(function(x, y) { return (x / y)|0; }),
	"%": $MakeOperator(function(x, y) { return (x % y)|0; }),
	"++": $MakeOperator(function(x, y) { return x + y; }),
	"&&": $MakeOperator(function(x, y) { return x && y; }),
	"||": $MakeOperator(function(x, y) { return x || y; }),
	"==": $MakeOperator(function(x, y) { return x == y; }),
	"/=": $MakeOperator(function(x, y) { return x != y; }),
	">=": $MakeOperator(function(x, y) { return x >= y; }),
	"<=": $MakeOperator(function(x, y) { return x <= y; }),
	">": $MakeOperator(function(x, y) { return x > y; }),
	"<": $MakeOperator(function(x, y) { return x < y; })
};

// TODO:
// $
// >> >>=
// << =<<
// . ? or <>

var $Prefix = {
	"-": $Partial(function(x) { return (-$Force(x))|0; }, 1, [])
};

// End of Prelude
// -----------------------------------------------
// Start of Foreign

var _print_foreign = function(thunk, $Bang) {
	var value = $Force(thunk);
	console.log(value);
	return $Unit;
};

var _print = $Partial(_print_foreign, 2, []);

var _putStr_foreign = function(thunk, $Bang) {
	var value = $Force(thunk);
	console.log(value);
	return $Unit;
}

var _putStr = $Partial(_putStr_foreign, 2, []);

var _show_foreign = function(thunk) {
	return value + "";
};

var _show = $Partial(_show_foreign, 1, []);

var _not_foreign = function(thunk) {
	return !$Force(thunk);
};

var _not = $Partial(_not_foreign, 1, []);

// End of Foreign
|]


