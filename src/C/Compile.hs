{-# LANGUAGE NamedFieldPuns #-}

module C.Compile where

import Data.List(intercalate)
import C.Prelude
import Compile

tab :: String -> String
tab "" = ""
tab str = '\t' : tab' str where
	tab' "\n" = "\n"
	tab' "" = ""
	tab' ('\n':cs) = '\n' : '\t' : tab' cs
	tab' (c : cs) = c : tab' cs

serialize :: I -> String
serialize (IName ('$':n)) = n
serialize (IName n) = "_" ++ n
serialize (IOperator op) = "Partial((void(*)())Operator" ++ concat (map speak op) ++ ", 2)" where
	speak '+' = "Plus"
	speak '-' = "Minus"
	speak '*' = "Star"
	speak '=' = "Equals"
	speak '?' = "Question"
	speak '>' = "More"
	speak '<' = "Less"
	speak '/' = "Slash"
	speak '^' = "Caret"
	speak '$' = "Dollar"
	speak '!' = "Bang"
	speak '&' = "And"
	speak '|' = "Or"
	speak '%' = "Percent"
	speak x = error $ "no known operator `" ++ show x ++ "`"
serialize (IInt i) = "MakeInt(" ++ i ++ ")"
serialize (IDecimal d) = "MakeDecimal(" ++ d ++ ")"
serialize (IString s) = "MakeString(" ++ s ++ ")"
serialize (ILiteral s) = s ++ "/*iliteral*/"
serialize (ICall f xs) = go (serialize f) xs
	where
	go fun [] = fun
	go fun (arg:args) = go ("Call(" ++ fun ++ ", " ++ serialize arg ++ ")") args
serialize (IForce x) = "Force(" ++ serialize x ++ ")"
serialize (IPartial funID capacity []) = "Partial( ( void(*)() )" ++ (serialize $ compileID funID) ++ ", " ++ show capacity ++ ")"
serialize (IPartial funID capacity args) = serialize $ (IPartial funID capacity []) `ICall` args
serialize (IConstructor name fields) = "Constructor(\"" ++ show name ++ "\", " ++ len ++ ", " ++ fieldNames ++ ", " ++ fieldValues ++ ")"
	where
	fieldNames = "(const char*[" ++ len ++ "]){" ++ intercalate ", " (map fieldNamer fields) ++ "}"
	fieldValues = "(Value[" ++ len ++ "]){" ++ intercalate ", " (map fieldValuer fields) ++ "}"
	fieldNamer (f, _) = "\"" ++ f ++ "\""
	fieldValuer (_, t) = serialize t
	len = show $ length fields
serialize (IDot left name) = "Dot(" ++ serialize left ++ ", \"" ++ show name ++ "\")"
serialize (IAssign v e) = serialize v ++ " = " ++ serialize e ++ ";\n"
serialize (IVar v) = "Value " ++ serialize v ++ ";\n"
serialize (IVarAssign v e) = "Value " ++ serialize v ++ " = " ++ serialize e ++ ";\n"
serialize (IIf c t []) = "if (Bool(" ++ serialize c ++ ")) {\n" ++ (tab $ concat $ map serialize t) ++ "}\n"
serialize (IIf c t e) = "if (Bool(" ++ serialize c ++ ")) {\n" ++ (tab $ concat $ map serialize t) ++ "} else {\n" ++ (tab $ concat $ map serialize e) ++ "}\n"
serialize (IWhile c b) = "while (Bool(" ++ serialize c ++ ")) {\n" ++ (tab $ concat $ map serialize b) ++ "}\n"
serialize (IDo e) = serialize e ++ ";\n"

serialize (IFunc funID args body) =
	"Value " ++ serialize (compileID funID) ++ "(" ++ intercalate ", " (map (("Value " ++) . serialize . IName) args) ++ ") {\n\tif(DEBUG)printf(\"@%d\\n\",__LINE__);\n" ++ (tab $ concat $ map serialize body) ++ "\treturn Unit;\n}\n"

serialize IBreak = "break;\n"
serialize (IReturn e) = "return VALIDATE(" ++ serialize e ++ ");\n"
serialize (ISequence s) = concat $ map serialize s
serialize (IComment comment) = "// " ++ comment ++ "\n"

headerGenerator :: CompiledFunction -> String
headerGenerator (CompiledFunction name _ n) = "Value " ++ (serialize $ compileID name) ++ "(" ++ intercalate ", " (take n $ repeat "Value") ++ ");\n"

serializeProgram :: Compiled I -> String
serializeProgram (Compiled headers value) =
	preludeSource ++
	"// forward feclarations\n" ++
	concat (map headerGenerator headers) ++
	"\n\n// function definitions\n" ++
	concat (map (\(CompiledFunction _ v _) -> serialize v) headers) ++
	"\n\n// Program\n\nint main() {\n" ++
	tab (preludeMain) ++ "\n" ++
	tab (serialize value) ++ "\n\n\t" ++
	"Force(Call(" ++ (serialize $ IName "main") ++ ", Bang));\n\treturn 0;\n}\n"
