{-# LANGUAGE NamedFieldPuns #-}

module Compile where

import Data.List(intercalate)
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
compileExpression gen (ExpressionFunc {arguments, funcBang, body}) = do
	let (fid, gen') = next gen
	(gen'', sbody) <- compileStatements gen' body
	let sargs = intercalate ", " $ map (token . fst) arguments
	let n = length arguments + if isNothing funcBang then 0 else 1
	addFunction fid $ "{nargs: " ++ show n ++ ", fun: function(" ++ sargs ++ ") { " ++ sbody ++ "}}"
	return (gen'', compileID fid)
