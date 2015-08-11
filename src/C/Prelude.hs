
{-# LANGUAGE QuasiQuotes #-}

module C.Prelude where

import Data.List(intercalate)

import Quotes

-- The prelude stores the little "runtime" we use to evaluate programs.

preludeSource :: String
preludeSource = [lit|
// THIS FILE GENERATED BY Nickel
// https://github.com/Nathan-Fenner/Ni
// <prelude>

#include "stdio.h"
#include "math.h"
#include "stdlib.h"
#include "string.h"

#define VALIDATE(x) _Validate(x, __LINE__)

const int DEBUG = 0;

struct Value;

typedef struct Value Value;

typedef struct {
	void (*function)(); // function pointer of function to Invoke()
	int capacity; // number of arguments total needed to invoke. 0 <= capacity
	int applied; // number of elements in arguments (applied <= capacity)
	Value * arguments; // an array of [capacity] arguments
} PartialValue;

typedef struct {
	Value * fun;
	Value * arg;
} CallValue;


// TODO: actually hash things
typedef struct {
	const char* structName;
	int count;
	const char** names;
	Value* values;
} ConstructorValue;

typedef enum {false, true} bool;

typedef enum {NO_TYPE, UNIT, BANG, PARTIAL, CALL, INTEGER, DOUBLE, CONSTRUCTOR} KIND;

typedef union {
	void * value;
	void (*function)();
	PartialValue partial;
	int intValue;
	double doubleValue;
	CallValue call;
	ConstructorValue constructor;
} ValueData;

struct Value {
	int kind;
	ValueData data;
};

typedef struct {
	int n;
	char ** names;
	void * values;
} TypeDescription;

const Value Unit = {UNIT};
const Value Bang = {BANG};

Value Call(Value fun, Value arg) {
	if (DEBUG) {
		printf("Call(%d, %d)\n", fun.kind, arg.kind);
	}
	Value call;
	call.kind = CALL;
	call.data.call.fun = malloc(sizeof(Value));
	*call.data.call.fun = fun;
	call.data.call.arg = malloc(sizeof(Value));
	*call.data.call.arg = arg;
	return call;
}

Value _Validate(Value value, int source) {
	if (DEBUG) {
		printf("Validate %d @%d\n", value.kind, source);
		if (value.kind == PARTIAL) {
			printf("Validate of PARTIAL\n");
			printf("\tapplied: %d\tcapacity: %d\n", value.data.partial.applied, value.data.partial.capacity);
			int i = 0;
			for (i = 0; i < value.data.partial.applied; i++) {
				printf("\t\targ %d has kind %d\n", i, value.data.partial.arguments[i]);
			}
		}
	}
	return value;
}

Value Invoke(void(*function)(), int argc, Value * argv) {
	if (DEBUG) {
		printf("Invoke(function, argc:%d, argv:%ld)\n", argc, argv);
		printf("Invoke args:\n");
		int i;
		for (i = 0; i < argc; i++) {
			printf("\t%d\n", argv[i].kind);
		}
	}
	switch (argc) {
|] ++ invokeBody ++ [lit|
	}
}

Value Force(Value value) {
	if (DEBUG) {
		printf("Force(%d)\n", value.kind);
	}
	switch (value.kind) {
	case PARTIAL:
		if (value.data.partial.applied == value.data.partial.capacity) {
			return Force(Invoke(value.data.partial.function, value.data.partial.applied, value.data.partial.arguments));
		} else {
			return value;
		}
	case CALL:;
		Value funValue = Force(*value.data.call.fun);
		if (funValue.kind != PARTIAL) {
			printf("expected PARTIAL (%d) in Force Call but got %d\n", PARTIAL, funValue.kind);
			exit(-1);
		}
		VALIDATE(funValue);
		PartialValue fun = funValue.data.partial;
		Value p;
		p.kind = PARTIAL;
		p.data.partial.capacity = fun.capacity;
		p.data.partial.function = fun.function;
		p.data.partial.applied = fun.applied + 1;
		//
		p.data.partial.arguments = malloc(sizeof(Value) * (p.data.partial.applied));
		int i;
		for (i = 0; i < fun.applied; i++) {
			p.data.partial.arguments[i] = fun.arguments[i];
		}
		p.data.partial.arguments[fun.applied] = *value.data.call.arg;
		return Force(VALIDATE(p));
	case UNIT:
	case BANG:
	case INTEGER:
	case CONSTRUCTOR:
		return value;
	}
	printf("unknown kind %d\n", value.kind);
	exit(-1);
	return value;
}

Value Partial(void (*function)(), int capacity) {
	Value v;
	v.kind = PARTIAL;
	v.data.partial.function = function;
	v.data.partial.capacity = capacity;
	v.data.partial.applied = 0;
	return v;
}

Value Constructor(const char * name, int len, const char ** names, Value * values) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	Value result;
	result.kind = CONSTRUCTOR;
	result.data.constructor.structName = name;
	result.data.constructor.count = len;
	result.data.constructor.names = malloc(sizeof(char*) * len);
	result.data.constructor.values = malloc(sizeof(Value) * len);
	int i;
	for (i = 0; i < len; i++) {
		result.data.constructor.names[i] = names[i];
		result.data.constructor.values[i] = values[i];
	}
	return result;
}

Value Dot(Value left, const char * field) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	if (left.kind != CONSTRUCTOR) {
		printf("Dot expected a constructor (%d) but got %d", CONSTRUCTOR, left.kind);
		exit(-1);
	}
	int i;
	for (i = 0; i < left.data.constructor.count; i++) {
		if (strcmp(field, left.data.constructor.fields[i]) == 0) {
			return left.data.constructor.values[i];
		}
	}
	printf("no such field %s in construct of type %s", field, left.data.constructor.name);
	exit(-1);
}

// value must be forced
bool Bool(Value value) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	// assert value.kind == INTEGER
	return value.data.intValue;
}

// value must be forced
int Integer(Value value) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	// assert value.kind == INTEGER
	return value.data.intValue;
}

// <boxers>
Value MakeInt(int n) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	Value v;
	v.kind = INTEGER;
	v.data.intValue = n;
	return v;
}

Value MakeDecimal(double d) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	Value v;
	v.kind = DOUBLE;
	v.data.doubleValue = d;
	return v;
}
// </boxers>

// <operators>
// int + int
Value OperatorPlus(Value left, Value right) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	Value v;
	v.kind = INTEGER;
	v.data.intValue = Integer(Force(left)) + Integer(Force(right));
	return VALIDATE(v);
}

Value OperatorPercent(Value left, Value right) {
	Value v;
	v.kind = INTEGER;
	v.data.intValue = Integer(Force(left)) % Integer(Force(right));
	return VALIDATE(v);
}

// int == int
Value OperatorEqualsEquals(Value left, Value right) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	Value v;
	v.kind = INTEGER;
	v.data.intValue = Integer(Force(left)) == Integer(Force(right));
	return VALIDATE(v);
}

// int /= int
Value OperatorSlashEquals(Value left, Value right) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	Value v;
	v.kind = INTEGER;
	v.data.intValue = Integer(Force(left)) != Integer(Force(right));
	return VALIDATE(v);
}

// int > int
Value OperatorMore(Value left, Value right) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	Value v;
	v.kind = INTEGER;
	v.data.intValue = Integer(Force(left)) > Integer(Force(right));
	return VALIDATE(v);
}

// int < int
Value OperatorLess(Value left, Value right) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	Value v;
	v.kind = INTEGER;
	v.data.intValue = Integer(Force(left)) < Integer(Force(right));
	return VALIDATE(v);
}

// int >= int
Value OperatorMoreEquals(Value left, Value right) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	Value v;
	v.kind = INTEGER;
	v.data.intValue = Integer(Force(left)) >= Integer(Force(right));
	return VALIDATE(v);
}

// int <= int
Value OperatorLessEquals(Value left, Value right) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	Value v;
	v.kind = INTEGER;
	v.data.intValue = Integer(Force(left)) <= Integer(Force(right));
	return VALIDATE(v);
}

// </operators>

// <built-in>

Value b_print(Value number, Value bang) {
	if (DEBUG) {
		printf("@%d\n", __LINE__);
	}
	int q = Integer(Force(number));
	printf("%d\n", q);
	return Unit;
}

// </built-in>

// </prelude>
|]

invokeBody :: String
invokeBody = concat $ map go [0..60] where
	go :: Int -> String
	go n = interp n [lit|
	case #:;
		Value (*fn#)($) = (Value(*)($))function;
		return fn#(%);
|]
	interp n str = concat $ map (ic n) str
	ic n '#' = show n
	ic n '$' = intercalate ", " $ map (const "Value") [1..n]
	ic n '%' = intercalate ", " $ map (\i -> interp i "argv[#]") [0..n-1]
	ic _ c = [c]


preludeMain :: String
preludeMain = [lit|
// Define the _print function for use in Nickel source
Value _print = Partial((void(*)())b_print, 2);
|]

