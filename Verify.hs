
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
statementAt (StatementStruct t _ _ _) = t

expressionAt :: Expression -> Token
expressionAt (ExpressionIdentifier t) = t
expressionAt (ExpressionIntegerLiteral t) = t
expressionAt (ExpressionDecimalLiteral t) = t
expressionAt (ExpressionStringLiteral t) = t
expressionAt (ExpressionBoolLiteral t) = t
expressionAt (ExpressionBang t) = t
expressionAt (ExpressionCall e _) = expressionAt e
expressionAt (ExpressionDot atom _) = expressionAt atom
expressionAt (ExpressionFunc { anonFuncToken = t }) = t
expressionAt (ExpressionOp left _ _) = expressionAt left
expressionAt (ExpressionPrefix op _) = op
expressionAt (ExpressionConstructor t _) = typeAt t

data Scope = Scope
	{ scopeReturnType :: Type -- return type
	, scopeTypes :: [(String, [String], [(String, Type)])] -- type names in scope
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

declareType :: String -> [String] -> [(String, Type)] -> Scope -> Scope
declareType struct generics args (Scope returns types scope) = Scope returns ((struct, generics, args) : types) scope

declareTypes :: [(String, [String], [(String, Type)])] -> Scope -> Scope
declareTypes [] scope = scope
declareTypes (p:ps) scope = declareTypes ps $ (\(x,y,z) -> declareType x y z) p scope

isTypeDeclared :: String -> Scope -> Bool
isTypeDeclared n (Scope _ types _) = n `elem` map (\(n',_,_) -> n') types

lookupTypeDeclaration :: Type -> Scope -> Either String [(String, Type)]
lookupTypeDeclaration structType (Scope _ types _) = case canonical of
	Left _ -> Left $ "The type `" ++ niceType structType ++ "` is not a struct-type"
	Right (CanonicalType name generics) -> case [ (generics', fields') | (name', generics', fields') <- types, name == name' ] of
		[] -> Left $ "There is no type with name `" ++ name ++ "`"
		((generics', fields'):_) -> case length generics' == length generics of
			False -> Left $ "struct type `" ++ name ++ "` expects " ++ show (length generics') ++ " type arguments, but it has been given " ++ show (length generics)
			True -> Right $ map (\(f, t) -> (,) f $ replaceTypes (zip generics' generics) t) fields'
	where
	canonical = canonicalType structType

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
getExpressionType scope (ExpressionCall funValue funArgs) = do
	funType <- getExpressionType scope funValue
	match funType funArgs
	where
	match :: Type -> [Expression] -> Check Type
	match (TypeGenerics generics fun) args = match' fun args (map token generics) []
	match fun args = match' fun args [] []
	match' fun [] _ sofar = return $ replaceTypes sofar fun
	match' (TypeArrow left right) (arg : rest) free sofar = do
		argType <- getExpressionType scope arg
		sofar' <- case unifyTypes argType left free of
			Left msg -> flunk (expressionAt arg) msg
			Right sofar' -> return sofar'
		sofar'' <- case unifyWith sofar sofar' of
			Left msg -> flunk (expressionAt arg) msg
			Right sofar'' -> return sofar''
		match' right rest free sofar''
	match' (TypeBangArrow _ right) (ExpressionBang _ : rest) free sofar = match' right rest free sofar
	match' (TypeBangArrow _ _) (arg : _) _ _ = flunk (expressionAt arg) $ "expected a `!` instead of value applied to object of function type"
	match' (TypeGenerics generics right) args@(arg:_) free sofar = case filter (`elem` free) (map token generics) of
		[] -> match' right args (map token generics ++ free) sofar
		shadowed -> flunk (expressionAt arg) $ "generics may not shadow each other (generic variable(s) " ++ intercalate ", " shadowed ++ ")"	
	match' fun (arg : _) _ _ = flunk (expressionAt arg) $ "cannot apply argument to object with non-function type `" ++ niceType fun ++ "`"

getExpressionType _scope (ExpressionBang bang) = flunk bang "a bang `!` outside of a matching function call is not allowed"
getExpressionType scope (ExpressionFunc _funcToken generics args bang returns body) = do
	let scopeBody = setReturn returnType $ addTypes args scope
	_ <- verifyStatementBlock scopeBody body
	Pass funcType
	where
	returnType' = case returns of
		Nothing -> makeType "Void"
		Just t -> t
	returnType = case bang of
		Nothing -> returnType'
		Just _ -> TypeBangArrow (Token "!" (FileEnd "*") Special) returnType'
	funcType' = go (map snd args) where
		go [] = returnType
		go (t : ts) = t `TypeArrow` go ts
	funcType = case generics of
		[] -> funcType'
		_ -> TypeGenerics generics funcType'
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
getExpressionType scope (ExpressionConstructor name fields) = case lookupTypeDeclaration name scope of
	Left msg -> flunk (typeAt name) $ msg
	Right fieldTypes -> do
		case filter (\(f,_) -> not $ f `elem` map (token . fst) fields) fieldTypes of
			[] -> return ()
			[one] -> flunk (typeAt name) $ "field `" ++ fst one ++ "` of type `" ++ niceType name ++ "` is never assigned"
			several -> flunk (typeAt name) $ "fields " ++ intercalate ", " (map (\n -> "`" ++ fst n ++ "`") several) ++ " of type `" ++ niceType name ++ "` are never assigned"
		if length fields == length fieldTypes then return () else flunk (typeAt name) $ "some fields for constructor of type `" ++ niceType name ++ "` are assigned twice"
		mapM_ (uncurry $ checkField fieldTypes) fields
		return name
	where
	checkField fieldTypes fieldName fieldValue = case lookUp (token fieldName) fieldTypes of
		Nothing -> flunk fieldName $ "type `" ++ niceType name ++ "` does not have a field called `" ++ token fieldName ++ "`"
		Just expect -> do
			actual <- getExpressionType scope fieldValue
			-- TODO: replace with type unifier
			assertTypeEqual actual expect (expressionAt fieldValue)
			return ()
	lookUp k m = case [v | (k', v) <- m, k == k'] of
		[] -> Nothing
		(v : _) -> Just v
getExpressionType scope (ExpressionDot left field) = do
	leftType <- getExpressionType scope left
	case lookupTypeDeclaration leftType scope of
		Left msg -> flunk field $ "can't index non-struct type `" ++ niceType leftType ++ "`: " ++ msg
		Right fields -> case lookUp (token field) fields of
			Nothing -> flunk field $ "type `" ++ niceType leftType ++ "` has no field called `" ++ show field ++ "`"
			Just t -> return t
	where
	lookUp k m = case [v | (k', v) <- m, k == k'] of
		[] -> Nothing
		(v : _) -> Just v


verifyTypeScope :: Scope -> Type -> Check ()
verifyTypeScope scope (TypeName t) = if token t `isTypeDeclared` scope then return () else flunk t ("type `" ++ token t ++ "` is not defined")
verifyTypeScope scope (TypeCall fun args) = do
	verifyTypeScope scope fun
	mapM_ (verifyTypeScope scope) args
verifyTypeScope scope (TypeBangArrow _ right) = verifyTypeScope scope right
verifyTypeScope scope (TypeArrow left right) = verifyTypeScope scope left >> verifyTypeScope scope right
verifyTypeScope scope (TypeGenerics generics value) = verifyTypeScope (declareTypes (map (\t -> (token t, [], [])) generics) scope) value

verifyStatementType :: Scope -> Statement -> Check Scope
verifyStatementType scope (StatementVarVoid nameToken varType) = do
	case lookUp nameToken (scopeStack scope) of
		Nothing -> return ()
		Just other -> flunk nameToken $ "variable `" ++ token nameToken ++ "` has already been declared at " ++ show other
	verifyTypeScope scope varType
	return $ addType nameToken varType scope
	where
	lookUp k m = case [k' | (k', _) <- m, token k == token k'] of
		[] -> Nothing
		(v : _) -> Just v
verifyStatementType scope (StatementVarAssign nameToken varType value) = do
	case lookUp nameToken (scopeStack scope) of
		Nothing -> return ()
		Just other -> flunk nameToken $ "variable `" ++ token nameToken ++ "` has already been declared at " ++ show other
	verifyTypeScope scope varType
	valueType <- getExpressionType scope value
	case unifyTypes varType valueType [] of
		Left msg -> flunk (expressionAt value) $ "illegal assignment: " ++ msg
		Right _ -> return ()
	return $ addType nameToken varType scope
	where
	lookUp k m = case [k' | (k', _) <- m, token k == token k'] of
		[] -> Nothing
		(v : _) -> Just v
verifyStatementType scope (StatementAssign nameToken value) = do
	valueType <- getExpressionType scope value
	case getType nameToken scope of
		Nothing -> flunk nameToken $ "variable `" ++ token nameToken ++ "` is undeclared or out of scope"
		Just expect -> case unifyTypes expect valueType [] of
			Left msg -> flunk (expressionAt value) $ "illegal assignment: " ++ msg
			Right _ -> return scope
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
verifyStatementType scope (StatementReturn returnToken Nothing) = case unifyTypes (mustReturn scope) (makeType "Void") [] of
	Left msg -> flunk returnToken $ "illegal return; expected type `" ++ niceType (mustReturn scope) ++ "`; " ++ msg 
	Right _ -> return scope
verifyStatementType scope (StatementReturn _returnToken (Just value)) = do
	valueType <- getExpressionType scope value
	case unifyTypes (mustReturn scope) valueType [] of
		Left msg -> flunk (expressionAt value) $ "illegal return value: " ++ msg
		Right _ -> return scope
verifyStatementType scope (StatementFunc _funcToken funcName generics arguments bang returns body) = do
	verifyTypeScope scope funcType
	let scopeNew = addType funcName funcType scope
	let scopeBody = setReturn returnType $ addTypes arguments $ declareTypes (map (\g -> (token g, [], [])) generics) $ scopeNew
	_ <- verifyStatementBlock scopeBody body
	return scopeNew
	where
	returnType' = case returns of
		Nothing -> makeType "Void"
		Just t -> t
	returnType = case bang of
		Nothing -> returnType'
		Just _ -> TypeBangArrow (Token "!" (FileEnd "*") Special) returnType'
	funcType' = go (map snd arguments) where
		go [] = returnType
		go (t:ts) = t `TypeArrow` go ts
	funcType = case generics of
		[] -> funcType'
		_ -> TypeGenerics generics funcType'
verifyStatementType scope StatementBreak{} = return scope
verifyStatementType scope (StatementLet letToken body) = do
	letBody <- mapM verifyLet body
	-- TODO: prevent duplicate names, types
	mapM_ notShadowed declaredTypes
	let newScope = addTypes (concat letBody) $ declareTypes declaredTypes scope
	_ <- mapM (verifyStatementType newScope) $ filter (not . isStruct) body
	return newScope
	where
	notShadowed (name, _, _) = case name `elem` map (\(x,_,_) -> x) (scopeTypes scope) of
		False -> return ()
		True -> flunk letToken $ "let block re-declares type `" ++ name ++ "`"
	isStruct StatementStruct{} = True
	isStruct _ = False
	declaredTypes :: [(String, [String], [(String, Type)])]
	declaredTypes = concat $ map go body where
		go (StatementStruct _ structName structGenerics structArgs) = [(token structName, map token structGenerics, map (\(f, t) -> (token f, t)) structArgs)]
		go _ = []
	verifyLet (StatementVarAssign varName varType _) = Pass [(varName, varType)]
	verifyLet (StatementFunc _funcToken funcName generics arguments bang returns _body) = Pass [(funcName, funcType)] where
		funcType' = go (map snd arguments) where
			go [] = returnType
			go (t:ts) = t `TypeArrow` go ts
		funcType = case generics of
			[] -> funcType'
			_ -> TypeGenerics generics funcType'
		returnType' = case returns of
			Nothing -> makeType "Void"
			Just t -> t
		returnType = case bang of
			Nothing -> returnType'
			Just _ -> TypeBangArrow (Token "!" (FileEnd "*") Special) returnType'
	verifyLet StatementStruct{} = Pass []
	verifyLet s = flunk (statementAt s) $ "only variable definitions, function declarations, and type definitions are allowed in let blocks"
verifyStatementType scope (StatementStruct _ structName generics fields)
	|token structName `isTypeDeclared` scope = flunk structName $ "type `" ++ token structName ++ "` has already been declared"
	|otherwise = return $ declareType (token structName) (map token generics) (map (\(f,t) -> (token f, t)) fields) scope

verifyStatementBlock :: Scope -> [Statement] -> Check Scope
verifyStatementBlock scope [] = return scope
verifyStatementBlock scope (s : ss) = do
	scope' <- verifyStatementType scope s
	verifyStatementBlock scope' ss

topScope :: Scope
topScope =
	Scope (makeType "Void")
		[ ("Void", [], []), ("Int", [], []), ("String", [], []), ("Bool", [], []), ("Integer", [], []) ]
		[ (Token "print" (FileEnd "^") Identifier, makeType "Int" `TypeArrow` TypeBangArrow (Token "!" (FileEnd "*") Special) (makeType "Void"))
		, (Token "putStr" (FileEnd "^") Identifier, makeType "String" `TypeArrow` TypeBangArrow (Token "!" (FileEnd "*") Special) (makeType "Void"))
		, (Token "not" (FileEnd "^") Identifier, makeType "Bool" `TypeArrow` makeType "Bool")
		, (Token "show" (FileEnd "^") Identifier, makeType "Int" `TypeArrow` makeType "String")
		, (Token "iAdd" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` (makeType "Integer" `TypeArrow` makeType "Integer") )
		, (Token "iSubtract" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` (makeType "Integer" `TypeArrow` makeType "Integer") )
		, (Token "iMultiply" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` (makeType "Integer" `TypeArrow` makeType "Integer") )
		, (Token "iDivide" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` (makeType "Integer" `TypeArrow` makeType "Integer") )
		, (Token "iMod" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` (makeType "Integer" `TypeArrow` makeType "Integer") )
		, (Token "iNegate" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` makeType "Integer" )
		, (Token "iPrint" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` TypeBangArrow (Token "!" (FileEnd "*") Special) (makeType "Void"))
		, (Token "iLess" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` (makeType "Integer" `TypeArrow` makeType "Bool") )
		, (Token "iEquals" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` (makeType "Integer" `TypeArrow` makeType "Bool") )
		, (Token "big" (FileEnd "^") Identifier, makeType "Int" `TypeArrow` makeType "Integer")
		]

verifyProgram :: Statement -> Check ()
verifyProgram program = verifyStatementType topScope program >> return ()