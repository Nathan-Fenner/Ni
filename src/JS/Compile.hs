{-# LANGUAGE NamedFieldPuns, QuasiQuotes #-}

module JS.Compile where

import Data.List(intercalate)

import JS.Prelude
import Compile

import Quotes

tab :: String -> String
tab "" = ""
tab str = '\t' : tab' str where
	tab' "\n" = "\n"
	tab' "" = ""
	tab' ('\n':cs) = '\n' : '\t' : tab' cs
	tab' (c : cs) = c : tab' cs
	
escape :: String -> String
escape s = "_" ++ s

serialize :: I -> String
serialize (IName n) = escape n
serialize (IInt i) = i
serialize (IDecimal i) = i
serialize (IString i) = i
serialize (IOperator op) = "$Operator[" ++ op ++ "]"
serialize (ILiteral s) = s
serialize (ICall f a) = "$Call(" ++ serialize f ++ ", [" ++ intercalate ", " (map serialize a) ++ "])"
serialize (IRemember x) = "$Remember(" ++ serialize x ++ ")"
serialize (IForce x) = "$Force(" ++ serialize x ++ ")"
serialize (IInvoke fun args) = serialize (compileID fun) ++ "(" ++ intercalate ", " (map serialize args) ++ ")"
serialize (IPartial funID capacity args) = "$Partial(" ++ serialize (compileID funID) ++ ", " ++ show capacity ++ ", [" ++ intercalate ", " (map serialize args) ++ "])"
serialize (IConstructor name fields) = "$Constructor(" ++ show name ++ ", {" ++ intercalate ", " (map field fields) ++ "})" where
	field (f, t) = f ++ ": " ++ serialize t
serialize (IDot left name) = "$Dot(" ++ serialize left ++ ", " ++ show name ++ ")"
serialize (IAssign v e) = serialize v ++ " = " ++ serialize e ++ ";\n"
serialize (IVar v) = "var " ++ serialize v ++ ";\n"
serialize (IVarAssign v e) = "var " ++ serialize v ++ " = " ++ serialize e ++ ";\n"
serialize (IIf c t []) = "if (" ++ serialize c ++ ") {\n" ++ (tab $ concat $ map serialize t) ++ "}\n"
serialize (IIf c t e) = "if (" ++ serialize c ++ ") {\n" ++ (tab $ concat $ map serialize t) ++ "} else {\n" ++ (tab $ concat $ map serialize e) ++ "}\n"
serialize (IWhile c b) = "while (" ++ serialize c ++ ") {\n" ++ (tab $ concat $ map serialize b) ++ "}\n"
serialize (IDo e) = serialize e ++ ";\n"
serialize (IFunc funID args body) =
	"function " ++ serialize (compileID funID) ++ "(" ++ intercalate ", " (map escape args) ++ ") {\n" ++ (tab $ concat $ map serialize body) ++ "\treturn $Unit;\n}\n"

serialize IBreak = "break;\n"
serialize (IReturn e) = "return " ++ serialize e ++ ";\n"
serialize (ISequence s) = concat $ map serialize s
serialize (IComment comment) = "// " ++ comment ++ "\n"

serializeProgram :: Compiled I -> String
serializeProgram (Compiled headers value) =
	preludeSource ++
	concat (map (\(CompiledFunction _ v _) -> serialize v) headers) ++ [lit|
// Program

try {
	// Define globals
	|] ++ tab (serialize value) ++ [lit|

	// Invoke main

	$Force($Call(_main, [$Bang]));
} catch (m) {
	console.log("An unexpected error occurred", m);
}
|]




