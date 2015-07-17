
module Verify where

import Control.Applicative
import Expression
import ParseType
import Lex

data Verify = Success | Failure [(Token, String)]

data Message = Message Token String deriving Show

data Check a = Flunk [Message] | Pass a deriving Show

instance Functor Check where
	fmap f (Pass x) = Pass (f x)
	fmap f (Flunk msg) = Flunk msg

instance Applicative Check where
	pure x = Pass x
	Pass f <*> Pass x = Pass (f x)
	Flunk m <*> Flunk m' = Flunk (m ++ m')
	Flunk m <*> _ = Flunk m
	_ <*> Flunk m = Flunk m

instance Monad Check where
	return = pure
	Pass x >>= f = f x
	Flunk m >>= _ = Flunk m

flunk :: Token -> String -> Check any
flunk token message = Flunk [Message token message]

statementAt :: Statement -> Token
statementAt (StatementAssign t _) = t
statementAt (StatementVarVoid t _) = t
statementAt (StatementVarAssign t _ _) = t
statementAt (StatementDo e) = expressionAt e
statementAt (StatementIf t _ _) = t
statementAt (StatementIfElse t _ _ _) = t
statementAt (StatementWhile t _ _) = t
statementAt (StatementReturn t _) = t
statementAt (StatementBreak t) = t
statementAt (StatementLet t _) = t
statementAt (StatementFunc {funcToken = t}) = t

expressionAt :: Expression -> Token
expressionAt (ExpressionIdentifier t) = t
expressionAt (ExpressionIntegerLiteral t) = t
expressionAt (ExpressionDecimalLiteral t) = t
expressionAt (ExpressionStringLiteral t) = t
expressionAt (ExpressionBang t) = t
expressionAt (ExpressionCall e _) = expressionAt e
expressionAt (ExpressionFunc { anonFuncToken = t }) = t
expressionAt (ExpressionOp left _ _) = expressionAt left
expressionAt (ExpressionPrefix op _) = op

data Scope = Scope Type {- return type -} [(Token, Type)] {- stack & globals -} deriving Show

getType :: Token -> Scope -> Maybe Type
getType name (Scope _ scope) = case filter (\(n, t) -> token n == token name) scope of
	[] -> Nothing
	((_, t):_) -> Just t

addType :: Token -> Type -> Scope -> Scope
addType varName varType (Scope returns scope) = Scope returns $ (varName, varType) : scope

addTypes :: [(Token, Type)] -> Scope -> Scope
addTypes [] scope = scope
addTypes ((n, t) : rest) scope = addTypes rest $ addType n t scope

mustReturn :: Scope -> Type
mustReturn (Scope r _) = r

setReturn :: Type -> Scope -> Scope
setReturn r (Scope _ ts) = Scope r ts

assertTypeEqual :: Type -> Type -> Token -> Check ()
assertTypeEqual left right at
	|left === right = Pass ()
	|otherwise = flunk at $ "got type `" ++ show left ++ "` but expected type `" ++ show right ++ "`"

getExpressionType :: Scope -> Expression -> Check Type
getExpressionType scope (ExpressionIdentifier name) = case getType name scope of
	Nothing -> flunk name $ "variable `" ++ token name ++ "` has not been declared or is not in scope"
	Just t -> Pass t
getExpressionType scope (ExpressionIntegerLiteral _) = Pass (makeType "Int")
getExpressionType scope (ExpressionDecimalLiteral _) = Pass (makeType "Float")
getExpressionType scope (ExpressionStringLiteral _) = Pass (makeType "String")
getExpressionType scope (ExpressionCall fun args) = do
	funType <- getExpressionType scope fun
	matchFuncType funType args
	where
	matchFuncType :: Type -> [Expression] -> Check Type
	matchFuncType t [] = Pass t
	matchFuncType (TypeBangArrow right) (ExpressionBang{} : rest) = matchFuncType right rest
	matchFuncType (TypeBangArrow _) (arg : _) = do
		flunk (expressionAt arg) $ "expected bang `!` but got expression " ++ show arg
	matchFuncType (TypeArrow left _) (ExpressionBang bang : rest) =
		flunk bang $ "was expecting an argument of type " ++ show left ++ ", not a bang `!`"
	matchFuncType (TypeArrow left right) (arg : rest) = do
		argType <- getExpressionType scope arg
		assertTypeEqual argType left (expressionAt arg)
		matchFuncType right rest
getExpressionType scope (ExpressionBang bang) = flunk bang "a bang `!` outside of a matching function call is not allowed"
getExpressionType scope (ExpressionFunc funcToken args bang returns body) = do
	-- TODO: check the body (after bringing the arguments into scope)
	Pass funcType
	where
	funcType = go (map snd args) returns where
		go [] Nothing = makeType "Void"
		go [] (Just r) = r
		go (t : ts) r = t `TypeArrow` go ts r
getExpressionType scope (ExpressionOp left op right) = do
	leftType <- getExpressionType scope left
	rightType <- getExpressionType scope right
	assertTypeEqual leftType leftExpect (expressionAt left)
	assertTypeEqual rightType rightExpect (expressionAt right)
	return $ returns
	where
	triple t = (makeType t, makeType t, makeType t)
	arithmetic = triple "Int"
	boolean = triple "Bool"
	(leftExpect, rightExpect, returns) = case token op of
		"+" -> arithmetic
		"-" -> arithmetic
		"*" -> arithmetic
		"/" -> arithmetic
		"%" -> arithmetic
		"&&" -> boolean
		"||" -> boolean
		"++" -> triple "String"
		_ -> error $ "compiler error: undefined infix operator `" ++ token op ++ "` (please contact the repository owner at github.com/Nathan-Fenner/Ni)"
getExpressionType scope (ExpressionPrefix op arg) = do
	argType <- getExpressionType scope arg
	assertTypeEqual argType expect (expressionAt arg)
	return returns
	where
	(expect, returns) = case token op of
		"-" -> (makeType "Int", makeType "Int")
		_ -> error $ "compiler error: undefined prefix operator `" ++ token op ++ "` (please contact the repository owner at github.com/Nathan-Fenner/Ni)"

verifyStatementType :: Scope -> Statement -> Check Scope
verifyStatementType scope (StatementVarVoid nameToken varType) = return $ addType nameToken varType scope
verifyStatementType scope (StatementVarAssign nameToken varType value) = do
	valueType <- getExpressionType scope value
	assertTypeEqual varType valueType (expressionAt value)
	return $ addType nameToken varType scope
verifyStatementType scope (StatementAssign nameToken value) = do
	valueType <- getExpressionType scope value
	case getType nameToken scope of
		Nothing -> flunk nameToken $ "variable `" ++ token nameToken ++ "` is undeclared or out of scope"
		Just expect -> do
			assertTypeEqual valueType expect (expressionAt value)
			return scope
verifyStatementType scope (StatementDo value) = do
	_ <- getExpressionType scope value
	return scope
verifyStatementType scope (StatementIf _ condition body) = do
	conditionType <- getExpressionType scope condition
	assertTypeEqual (makeType "Bool") conditionType (expressionAt condition)
	verifyStatementBlock scope body
	return scope
verifyStatementType scope (StatementIfElse _ condition bodyThen bodyElse) = do
	conditionType <- getExpressionType scope condition
	assertTypeEqual (makeType "Bool") conditionType (expressionAt condition)
	verifyStatementBlock scope bodyThen
	verifyStatementBlock scope bodyElse
	return scope
verifyStatementType scope (StatementWhile _ condition body) = do
	conditionType <- getExpressionType scope condition
	assertTypeEqual (makeType "Bool") conditionType (expressionAt condition)
	verifyStatementBlock scope body
	return scope
verifyStatementType scope (StatementReturn returnToken Nothing) = do
	assertTypeEqual (mustReturn scope) (makeType "Void") returnToken
	return scope
verifyStatementType scope (StatementReturn returnToken (Just value)) = do
	valueType <- getExpressionType scope value
	assertTypeEqual (mustReturn scope) valueType (expressionAt value)
	return scope
verifyStatementType scope (StatementFunc _funcToken funcName arguments _bang returns body) = do
	let scopeNew = addType funcName funcType scope
	let scopeBody = setReturn returnType $ addTypes arguments scopeNew
	verifyStatementBlock scopeBody body
	return scopeNew
	where
	returnType = case returns of
		Nothing -> makeType "Void"
		Just t -> t
	funcType = go (map snd arguments) where
		go [] = returnType
		go (t:ts) = t `TypeArrow` go ts
verifyStatementType scope StatementBreak{} = return scope
verifyStatementType scope (StatementLet _ body) = do
	letBody <- mapM verifyLet body
	-- TODO: prevent duplicate names
	let newScope = addTypes (concat letBody) scope
	_ <- mapM (verifyStatementType newScope) body
	return newScope
	where
	verifyLet s@StatementAssign{} = Pass []
	verifyLet s@(StatementVarAssign varName varType _) = Pass [(varName, varType)]
	verifyLet s@(StatementFunc _funcToken funcName arguments _bang returns body) = Pass [(funcName, funcType)] where
		funcType = go (map snd arguments) where
			go [] = returnType
			go (t:ts) = t `TypeArrow` go ts
		returnType = case returns of
			Nothing -> makeType "Void"
			Just t -> t
	verifyLet s = flunk (statementAt s) $ "only assignments, variable definitions, and function declarations are allowed in let blocks"
	

verifyStatementBlock :: Scope -> [Statement] -> Check Scope
verifyStatementBlock scope [] = return scope
verifyStatementBlock scope (s : ss) = do
	scope' <- verifyStatementType scope s
	verifyStatementBlock scope' ss
