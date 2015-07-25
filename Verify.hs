
module Verify where

import Control.Applicative
import Expression hiding(returnType, arguments, funcName, body)
import Data.List(intercalate)
import ParseType
import Lex

data Verify = Success | Failure [(Token, String)]

data Message = Message Token String deriving Show

data Check a = Flunk [Message] | Pass a deriving Show

instance Functor Check where
	fmap f (Pass x) = Pass (f x)
	fmap _ (Flunk msg) = Flunk msg

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
flunk atToken message = Flunk [Message atToken message]

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
statementAt (StatementStruct t _ _) = t

expressionAt :: Expression -> Token
expressionAt (ExpressionIdentifier t) = t
expressionAt (ExpressionIntegerLiteral t) = t
expressionAt (ExpressionDecimalLiteral t) = t
expressionAt (ExpressionStringLiteral t) = t
expressionAt (ExpressionBoolLiteral t) = t
expressionAt (ExpressionBang t) = t
expressionAt (ExpressionCall e _) = expressionAt e
expressionAt (ExpressionFunc { anonFuncToken = t }) = t
expressionAt (ExpressionOp left _ _) = expressionAt left
expressionAt (ExpressionPrefix op _) = op
expressionAt (ExpressionConstructor t _) = t

data Scope = Scope
	{ scopeReturnType :: Type -- return type
	, scopeTypes :: [(String, [(String, Type)])] -- type names in scope
	, scopeStack :: [(Token, Type)] -- stack & globals
	} deriving Show

getType :: Token -> Scope -> Maybe Type
getType name (Scope _ _ scope) = case filter (\(n, _) -> token n == token name) scope of
	[] -> Nothing
	((_, t):_) -> Just t

addType :: Token -> Type -> Scope -> Scope
addType varName varType (Scope returns types scope) = Scope returns types $ (varName, varType) : scope

addTypes :: [(Token, Type)] -> Scope -> Scope
addTypes [] scope = scope
addTypes ((n, t) : rest) scope = addTypes rest $ addType n t scope

declareType :: String -> [(String, Type)] -> Scope -> Scope
declareType struct args (Scope returns types scope) = Scope returns ((struct, args) : types) scope

declareTypes :: [(String, [(String, Type)])] -> Scope -> Scope
declareTypes [] scope = scope
declareTypes (p:ps) scope = declareTypes ps $ uncurry declareType p scope

isTypeDeclared :: String -> Scope -> Bool
isTypeDeclared n (Scope _ types _) = n `elem` map fst types

lookupTypeDeclaration :: String -> Scope -> Maybe [(String, Type)]
lookupTypeDeclaration name (Scope _ types _) = case [ fields | (name', fields) <- types, name == name' ] of
	[] -> Nothing
	(fields:_) -> Just fields

mustReturn :: Scope -> Type
mustReturn (Scope r _ _) = r

setReturn :: Type -> Scope -> Scope
setReturn r (Scope _ ks ts) = Scope r ks ts

assertTypeEqual :: Type -> Type -> Token -> Check ()
assertTypeEqual left right atToken
	|left === right = Pass ()
	|otherwise = flunk atToken $ "got type `" ++ niceType left ++ "` but expected type `" ++ niceType right ++ "`"

getExpressionType :: Scope -> Expression -> Check Type
getExpressionType scope (ExpressionIdentifier name) = case getType name scope of
	Nothing -> flunk name $ "variable `" ++ token name ++ "` has not been declared or is not in scope"
	Just t -> Pass t
getExpressionType _scope (ExpressionIntegerLiteral _) = Pass (makeType "Int")
getExpressionType _scope (ExpressionDecimalLiteral _) = Pass (makeType "Float")
getExpressionType _scope (ExpressionStringLiteral _) = Pass (makeType "String")
getExpressionType _scope (ExpressionBoolLiteral _) = Pass (makeType "Bool")
getExpressionType scope (ExpressionCall fun args) = do
	funType <- getExpressionType scope fun
	matchFuncType funType funType args
	where
	matchFuncType :: Type -> Type -> [Expression] -> Check Type
	matchFuncType _ t [] = Pass t
	matchFuncType wt (TypeBangArrow right) (ExpressionBang{} : rest) = matchFuncType wt right rest
	matchFuncType _ (TypeBangArrow _) (arg : _) = do
		flunk (expressionAt arg) $ "expected bang `!` but got expression " ++ show arg
	matchFuncType _ (TypeArrow left _) (ExpressionBang bang : _) =
		flunk bang $ "was expecting an argument of type `" ++ niceType left ++ "`, not a bang `!`"
	matchFuncType wt (TypeArrow left right) (arg : rest) = do
		argType <- getExpressionType scope arg
		assertTypeEqual argType left (expressionAt arg)
		matchFuncType wt right rest
	matchFuncType wt t (arg : _) = flunk (expressionAt arg) $ "got expression " ++ show arg ++ " applied as an argument to function " ++ show fun ++ " : `" ++ niceType wt ++ "` when none were expected `" ++ niceType t ++ "`"
getExpressionType _scope (ExpressionBang bang) = flunk bang "a bang `!` outside of a matching function call is not allowed"
getExpressionType scope (ExpressionFunc _funcToken args bang returns body) = do
	let scopeBody = setReturn returnType $ addTypes args scope
	_ <- verifyStatementBlock scopeBody body
	Pass funcType
	where
	returnType' = case returns of
		Nothing -> makeType "Void"
		Just t -> t
	returnType = case bang of
		Nothing -> returnType'
		Just _ -> TypeBangArrow returnType'
	funcType = go (map snd args) where
		go [] = returnType
		go (t : ts) = t `TypeArrow` go ts
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
	compares = (makeType "Int", makeType "Int", makeType "Bool")
	(leftExpect, rightExpect, returns) = case token op of
		"+" -> arithmetic
		"-" -> arithmetic
		"*" -> arithmetic
		"/" -> arithmetic
		"%" -> arithmetic
		"&&" -> boolean
		"||" -> boolean
		"++" -> triple "String"
		"==" -> compares
		"/=" -> compares
		"<=" -> compares
		">=" -> compares
		"<" -> compares
		">" -> compares
		_ -> error $ "compiler error: undefined infix operator `" ++ token op ++ "` (please contact the repository owner at github.com/Nathan-Fenner/Ni)"
getExpressionType scope (ExpressionPrefix op arg) = do
	argType <- getExpressionType scope arg
	assertTypeEqual argType expect (expressionAt arg)
	return returns
	where
	(expect, returns) = case token op of
		"-" -> (makeType "Int", makeType "Int")
		_ -> error $ "compiler error: undefined prefix operator `" ++ token op ++ "` (please contact the repository owner at github.com/Nathan-Fenner/Ni)"
getExpressionType scope (ExpressionConstructor name fields) = case lookupTypeDeclaration (token name) scope of
	Nothing -> flunk name $ "type `" ++ token name ++ "` is not declared"
	Just fieldTypes -> do
		case filter (\(f,_) -> not $ f `elem` map (token . fst) fields) fieldTypes of
			[] -> return ()
			[one] -> flunk name $ "field `" ++ fst one ++ "` of type `" ++ token name ++ "` is never assigned"
			several -> flunk name $ "fields " ++ intercalate ", " (map (\n -> "`" ++ fst n ++ "`") several) ++ " of type `" ++ token name ++ "` are never assigned"
		if length fields == length fieldTypes then return () else flunk name $ "some fields for constructor of type `" ++ token name ++ "` are assigned twice"
		mapM_ (uncurry $ checkField fieldTypes) fields
		return (makeType $ token name)
	where
	checkField fieldTypes fieldName fieldValue = case lookUp (token fieldName) fieldTypes of
		Nothing -> flunk fieldName $ "type `" ++ token name ++ "` does not have a field called `" ++ token fieldName ++ "`"
		Just expect -> do
			actual <- getExpressionType scope fieldValue
			assertTypeEqual actual expect (expressionAt fieldValue)
			return ()
	lookUp k m = case [v | (k', v) <- m, k == k'] of
		[] -> Nothing
		(v : _) -> Just v



verifyTypeScope :: Scope -> Type -> Check ()
verifyTypeScope scope (TypeName t) = if token t `isTypeDeclared` scope then return () else flunk t ("type `" ++ token t ++ "` is not defined")
verifyTypeScope scope (TypeCall fun args) = do
	verifyTypeScope scope fun
	mapM_ (verifyTypeScope scope) args
verifyTypeScope scope (TypeBangArrow right) = verifyTypeScope scope right
verifyTypeScope scope (TypeArrow left right) = verifyTypeScope scope left >> verifyTypeScope scope right

verifyStatementType :: Scope -> Statement -> Check Scope
verifyStatementType scope (StatementVarVoid nameToken varType) = do
	verifyTypeScope scope varType
	return $ addType nameToken varType scope
verifyStatementType scope (StatementVarAssign nameToken varType value) = do
	verifyTypeScope scope varType
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
	_ <- verifyStatementBlock scope body
	return scope
verifyStatementType scope (StatementIfElse _ condition bodyThen bodyElse) = do
	conditionType <- getExpressionType scope condition
	assertTypeEqual (makeType "Bool") conditionType (expressionAt condition)
	_ <- verifyStatementBlock scope bodyThen
	_ <- verifyStatementBlock scope bodyElse
	return scope
verifyStatementType scope (StatementWhile _ condition body) = do
	conditionType <- getExpressionType scope condition
	assertTypeEqual (makeType "Bool") conditionType (expressionAt condition)
	_ <- verifyStatementBlock scope body
	return scope
verifyStatementType scope (StatementReturn returnToken Nothing) = do
	assertTypeEqual (mustReturn scope) (makeType "Void") returnToken
	return scope
verifyStatementType scope (StatementReturn _returnToken (Just value)) = do
	valueType <- getExpressionType scope value
	assertTypeEqual (mustReturn scope) valueType (expressionAt value)
	return scope
verifyStatementType scope (StatementFunc _funcToken funcName arguments bang returns body) = do
	verifyTypeScope scope funcType
	let scopeNew = addType funcName funcType scope
	let scopeBody = setReturn returnType $ addTypes arguments scopeNew
	_ <- verifyStatementBlock scopeBody body
	return scopeNew
	where
	returnType' = case returns of
		Nothing -> makeType "Void"
		Just t -> t
	returnType = case bang of
		Nothing -> returnType'
		Just _ -> TypeBangArrow returnType'
	funcType = go (map snd arguments) where
		go [] = returnType
		go (t:ts) = t `TypeArrow` go ts
verifyStatementType scope StatementBreak{} = return scope
verifyStatementType scope (StatementLet _ body) = do
	letBody <- mapM verifyLet body
	-- TODO: prevent duplicate names, types
	let newScope = addTypes (concat letBody) $ declareTypes declaredTypes scope
	_ <- mapM (verifyStatementType newScope) body
	return newScope
	where
	declaredTypes = concat $ map go body where
		go (StatementStruct _ structName structArgs) = [(token structName, map (\(f, t) -> (token f, t)) structArgs)]
		go _ = []
	verifyLet (StatementVarAssign varName varType _) = Pass [(varName, varType)]
	verifyLet (StatementFunc _funcToken funcName arguments bang returns _body) = Pass [(funcName, funcType)] where
		funcType = go (map snd arguments) where
			go [] = returnType
			go (t:ts) = t `TypeArrow` go ts
		returnType' = case returns of
			Nothing -> makeType "Void"
			Just t -> t
		returnType = case bang of
			Nothing -> returnType'
			Just _ -> TypeBangArrow returnType'
	verifyLet StatementStruct{} = Pass []
	verifyLet s = flunk (statementAt s) $ "only variable definitions, function declarations, and type definitions are allowed in let blocks"
verifyStatementType scope (StatementStruct _ structName fields)
	|token structName `isTypeDeclared` scope = flunk structName $ "type `" ++ token structName ++ "` has already been declared"
	|otherwise = return $ declareType (token structName) (map (\(f,t) -> (token f, t)) fields) scope

verifyStatementBlock :: Scope -> [Statement] -> Check Scope
verifyStatementBlock scope [] = return scope
verifyStatementBlock scope (s : ss) = do
	scope' <- verifyStatementType scope s
	verifyStatementBlock scope' ss

topScope :: Scope
topScope =
	Scope (makeType "Void")
		[ ("Void", []), ("Int", []), ("String", []), ("Bool", []) ]
		[ (Token "print" (FileEnd "^") Identifier, makeType "Int" `TypeArrow` TypeBangArrow (makeType "Void"))
		, (Token "putStr" (FileEnd "^") Identifier, makeType "String" `TypeArrow` TypeBangArrow (makeType "Void"))
		]

verifyProgram :: Statement -> Check ()
verifyProgram program = verifyStatementType topScope program >> return ()