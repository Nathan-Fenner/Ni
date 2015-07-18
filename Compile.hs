{-# LANGUAGE NamedFieldPuns #-}

module Compile where

import Data.List(intercalate, sort)
import Control.Applicative
import Expression
import Lex
import ParseType

newtype FunctionID = FunctionID Int deriving (Show, Eq)

data IDGenerator = IDGenerator{next :: (FunctionID, IDGenerator)}

functionIDGenerator :: Int -> IDGenerator
functionIDGenerator n = IDGenerator{next = (FunctionID n, functionIDGenerator (n + 1))}

newGenerator :: IDGenerator
newGenerator = functionIDGenerator 0

data CompiledFunction = CompiledFunction FunctionID String deriving Show

data Compiled a = Compiled [CompiledFunction] a deriving Show

instance Functor Compiled where
	fmap f (Compiled x a) = Compiled x (f a)

instance Applicative Compiled where
	pure x = Compiled [] x
	Compiled xs f <*> Compiled ys a = Compiled (xs ++ ys) (f a)

instance Monad Compiled where
	return = pure
	Compiled fs a >>= f = let Compiled gs b = f a in Compiled (gs ++ fs) b

newID :: Compiled a -> Compiled FunctionID
newID (Compiled fs _)  = Compiled fs (FunctionID (length fs))

addFunction :: FunctionID -> String -> Compiled ()
addFunction fid body = Compiled [CompiledFunction fid body] ()

compileID :: FunctionID -> String
compileID (FunctionID n) = "_fun" ++ show n

----

mapGen :: IDGenerator -> [Expression] -> Compiled (IDGenerator, [String])
mapGen gen [] = return (gen, [])
mapGen gen (x:xs) = do
	(gen', x') <- compileExpression gen x
	(gen'', xs') <- mapGen gen' xs
	return (gen'', x' : xs')

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

nub' :: [String] -> [String]
nub' list = go $ sort list where
	go [] = []
	go (x:x':xs)
		|x == x' = go (x:xs)
	go (x:xs) = x : go xs

less :: [String] -> [String] -> [String]
less list remove = filter (\l -> not $ l `elem` remove) list

containedVariables :: Expression -> [String]
containedVariables (ExpressionIdentifier name) = [token name]
containedVariables ExpressionIntegerLiteral{} = []
containedVariables ExpressionDecimalLiteral{} = []
containedVariables ExpressionStringLiteral{} = []
containedVariables ExpressionBang{} = []
containedVariables (ExpressionCall fun args) = nub' $ containedVariables fun ++ concat (map containedVariables args)
containedVariables ExpressionFunc{arguments, body} = containedVariables' body `less` map (token . fst) arguments
containedVariables (ExpressionOp left _ right) = nub' $ containedVariables left ++ containedVariables right
containedVariables (ExpressionPrefix _ arg) = containedVariables arg

containedVariables' :: [Statement] -> [String]
containedVariables' (StatementVarVoid name _ : ss) = containedVariables' ss `less` [token name]
containedVariables' (StatementVarAssign name _ _ : ss) = containedVariables' ss `less` [token name]
containedVariables' (StatementDo e : ss) = nub' $ containedVariables e ++ containedVariables' ss
containedVariables' (StatementIf _ con body : ss) =
	nub' $ containedVariables con ++ containedVariables' body ++ containedVariables' ss
containedVariables' (StatementIfElse _ con bodyThen bodyElse : ss) =
	nub' $ containedVariables con ++ containedVariables' bodyThen ++ containedVariables' bodyElse ++ containedVariables' ss
containedVariables' (StatementWhile _ con body : ss) =
	nub' $ containedVariables con ++ containedVariables' body ++ containedVariables' ss
containedVariables' (StatementReturn _ (Just e) : ss) = nub' $ containedVariables e ++ containedVariables' ss
containedVariables' (StatementReturn _ Nothing : ss) = containedVariables' ss
containedVariables' (StatementBreak _ : ss) = containedVariables' ss
containedVariables' (StatementLet _ body : ss) =
	(nub' $ containedVariables' body ++ containedVariables' ss) `less` defined where
	defined = [ token name | StatementVarVoid name _ <- body ]
		++ [ token name | StatementVarAssign name _ _ <- body ]
		++ [ token funcName | StatementFunc{ funcName } <- body ]
containedVariables' (StatementFunc{ funcName, argumentsStatement, bodyStatement } : ss) =
	(nub' $ containedVariables' bodyStatement) `less` ( token funcName : map (token . fst) argumentsStatement)
containedVariables' [] = []

compileStatements = undefined

compileExpression :: IDGenerator -> Expression -> Compiled (IDGenerator, String)
compileExpression gen (ExpressionIdentifier name) = return (gen, token name)
compileExpression gen (ExpressionIntegerLiteral value) = return (gen, token value)
compileExpression gen (ExpressionDecimalLiteral value) = return (gen, token value)
compileExpression gen (ExpressionStringLiteral value) = return (gen, show $ token value)
compileExpression gen (ExpressionBang _) = return (gen, "BANG")
compileExpression gen (ExpressionCall fun args) = do
	(gen', fun') <- compileExpression gen fun
	(gen'', args') <- mapGen gen' args
	let sargs = "[" ++ intercalate ", " args' ++ "]"
	return (gen'', "CALL(" ++ fun' ++ ", " ++ sargs ++ ")")
compileExpression gen e@ExpressionFunc{arguments, funcBang, body} = do
	let (fid, gen') = next gen
	(gen'', sbody) <- compileStatements gen' body
	let extraArguments = containedVariables e
	let allArguments = extraArguments ++ map (token . fst) arguments
	let sargs = intercalate ", " allArguments
	let cargs = "[" ++ intercalate ", " extraArguments ++ "]"
	let n = length allArguments + if isNothing funcBang then 0 else 1
	addFunction fid $ "{nargs: " ++ show n ++ ", fun: function(" ++ sargs ++ ") { " ++ sbody ++ "}}"
	return (gen'', "CALL(" ++ compileID fid ++ ", " ++ cargs ++ ")")
compileExpression gen (ExpressionOp left op right) = do
	(gen', left') <- compileExpression gen left
	(gen'', right') <- compileExpression gen' right
	let funName = case token op of
		"+" -> "ADD"
		"-" -> "SUBTRACT"
		"*" -> "MULTIPLY"
		"/" -> "DIVIDE"
		"%" -> "MODULO"
		"++" -> "CONCAT"
		o -> error $ "programming error: infix operator `" ++ o ++ "` does not exist"
	return $ (gen'', funName ++ "(" ++ left' ++ ", " ++ right' ++ ")")
compileExpression gen (ExpressionPrefix op arg) = do
	(gen', arg') <- compileExpression gen arg
	let funName = case token op of
		"-" -> "NEGATE"
		o -> error $ "programming error: prefix operator `" ++ o ++ "` does not exist"
	return $ (gen', funName ++ "(" ++ arg' ++ ")")
