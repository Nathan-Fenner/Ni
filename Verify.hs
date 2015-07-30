
{-# LANGUAGE NamedFieldPuns #-}

module Verify where

import Control.Applicative
import Expression hiding(returnType, arguments, body)
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

data AssignState = AssignFinal | AssignNot | Assigned deriving (Show, Eq)

data Scope = Scope
	{ scopeReturnType :: Type -- return type
	, scopeTypes :: [(String, [String], [(String, Type)])] -- type names in scope
	, scopeStackAssign :: [(AssignState, Token, Type)] -- stack & globals
	} deriving Show

scopeStack :: Scope -> [(Token, Type)]
scopeStack = map (\(_,b,c) -> (b, c)) . scopeStackAssign

getType :: Token -> Scope -> Maybe Type
getType name scope = case filter (\(n, _) -> token n == token name) (scopeStack scope) of
	[] -> Nothing
	((_, t):_) -> Just t

addVariable :: AssignState -> Token -> Type -> Scope -> Scope
addVariable assignState varName varType (Scope returns types scope) = Scope returns types $ (assignState, varName, varType) : scope

addVariables :: AssignState -> [(Token, Type)] -> Scope -> Scope
addVariables _ [] scope = scope
addVariables assignState ((n, t) : rest) scope = addVariables assignState rest $ addVariable assignState n t scope

tryAssignment :: Token -> Scope -> Check Scope
tryAssignment var scope = do
	fixedUp <- fixUp (scopeStackAssign scope)
	return $ scope{ scopeStackAssign = fixedUp }
	where
	fixUp [] = return []
	fixUp (triple@(assignState, varName, varType) : rest)
		|token varName == token var = case assignState of
			AssignFinal -> flunk var $ "variable `" ++ token var ++ "` is final and cannot be assigned to"
			Assigned -> return $ (Assigned, varName, varType) : rest
			AssignNot -> return $ (Assigned, varName, varType) : rest
		|otherwise = do
			rest' <- fixUp rest
			return $ triple : rest'

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
	let scopeBody = setReturn returnType $ addVariables AssignFinal args scope
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

-- Checks that declarations are unique
verifyStatementTypeDeclare :: Scope -> Statement -> Check Scope
verifyStatementTypeDeclare scope statement@(StatementVarVoid varName varType) = do
	case lookUp varName (scopeStack scope) of
		Nothing -> return ()
		Just other -> flunk varName $ "variable `" ++ token varName ++ "` has already been declared at " ++ show other
	let newScope = addVariable AssignNot varName varType scope
	verifyStatementType scope statement
	return newScope
	where
	lookUp k m = case [k' | (k', _) <- m, token k == token k'] of
		[] -> Nothing
		(v : _) -> Just v
verifyStatementTypeDeclare scope statement@(StatementVarAssign varName varType _) = do
	case lookUp varName (scopeStack scope) of
		Nothing -> return ()
		Just other -> flunk varName $ "variable `" ++ token varName ++ "` has already been declared at " ++ show other
	let newScope = addVariable Assigned varName varType scope
	verifyStatementType scope statement
	return newScope
	where
	lookUp k m = case [k' | (k', _) <- m, token k == token k'] of
		[] -> Nothing
		(v : _) -> Just v
verifyStatementTypeDeclare scope statement@(StatementAssign varName _) = do
	newScope <- tryAssignment varName scope
	verifyStatementType scope statement
	return newScope
verifyStatementTypeDeclare scope statement@StatementFunc{funcName, genericsStatement, argumentsStatement, returnTypeStatement, funcBangStatement} = do
	case lookUp funcName (scopeStack scope) of
		Nothing -> return ()
		Just other -> flunk funcName $ "variable `" ++ token funcName ++ "` has already been declared at " ++ show other
	let newScope = addVariable AssignFinal funcName funcType scope
	verifyStatementType newScope statement
	return newScope
	where
	lookUp k m = case [k' | (k', _) <- m, token k == token k'] of
		[] -> Nothing
		(v : _) -> Just v
	returnType' = case returnTypeStatement of
		Nothing -> makeType "Void"
		Just t -> t
	returnType = case funcBangStatement of
		Nothing -> returnType'
		Just _ -> TypeBangArrow (Token "!" (FileEnd "*") Special) returnType'
	funcType' = go (map snd argumentsStatement) where
		go [] = returnType
		go (t:ts) = t `TypeArrow` go ts
	funcType = case genericsStatement of
		[] -> funcType'
		_ -> TypeGenerics genericsStatement funcType'
verifyStatementTypeDeclare scope (StatementLet letToken body) = do
	letBody <- mapM verifyLet body
	mapM_ notShadowed declaredTypes
	allUnique $ map (\(name, _, _) -> name) declaredTypes
	allUniqueVariable newNames
	allNewVariable newNames
	let newScope = addVariables AssignFinal (concat letBody) $ declareTypes declaredTypes scope
	-- TODO: check that declared structs are correct
	mapM_ (verifyStatementType newScope) body
	return newScope
	where
	notShadowed (name, _, _) = case name `elem` map (\(x,_,_) -> x) (scopeTypes scope) of
		False -> return ()
		True -> flunk letToken $ "let block re-declares type `" ++ name ++ "`"
	allUnique :: [String] -> Check ()
	allUnique [] = return ()
	allUnique (first:rest)
		|first `elem` rest = flunk letToken $ "let block defines type `" ++ first ++ "` multiple times"
		|otherwise = allUnique rest
	allNewVariable :: [Token] -> Check ()
	allNewVariable [] = return ()
	allNewVariable (name:rest)
		|token name `elem` map (\(name', _, _) -> name') (scopeTypes scope) = flunk name $ "variable `" ++ token name ++ "` has already been declared"
		|otherwise = allNewVariable rest
	allUniqueVariable :: [Token] -> Check ()
	allUniqueVariable [] = return ()
	allUniqueVariable (first:rest)
		|token first `elem` map token rest = flunk first $ "let block defines variable `" ++ token first ++ "` multiple times"
		|otherwise = allUniqueVariable rest
	declaredTypes :: [(String, [String], [(String, Type)])]
	declaredTypes = concat $ map go body where
		go (StatementStruct _ structName structGenerics structArgs) = [(token structName, map token structGenerics, map (\(f, t) -> (token f, t)) structArgs)]
		go _ = []
	newNames :: [Token]
	newNames = concat $ map nameOf body where
		nameOf StatementFunc{Expression.funcName} = [funcName]
		nameOf (StatementVarAssign varName _ _) = [varName]
		nameOf (StatementVarVoid varName _) = [varName]
		nameOf StatementStruct{} = []
		nameOf _ = error "invalid statement type in let-body"
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
verifyStatementTypeDeclare scope statement@(StatementStruct _ structName generics fields)
	|token structName `isTypeDeclared` scope = flunk structName $ "type `" ++ token structName ++ "` has already been declared"
	|otherwise = do
		let newScope = declareType (token structName) (map token generics) (map (\(f,t) -> (token f, t)) fields) scope
		verifyStatementType newScope statement
		return newScope

-- The catch-all for the rest:
verifyStatementTypeDeclare scope statement = verifyStatementType scope statement >> return scope

verifyStatementType :: Scope -> Statement -> Check ()
verifyStatementType scope (StatementVarVoid _ varType) = do
	verifyTypeScope scope varType
	return ()
verifyStatementType scope (StatementVarAssign _ varType value) = do
	verifyTypeScope scope varType
	valueType <- getExpressionType scope value
	case unifyTypes varType valueType [] of
		Left msg -> flunk (expressionAt value) $ "illegal assignment: " ++ msg
		Right _ -> return ()
	return ()
verifyStatementType scope (StatementAssign nameToken value) = do
	valueType <- getExpressionType scope value
	case getType nameToken scope of
		Nothing -> flunk nameToken $ "variable `" ++ token nameToken ++ "` is undeclared or out of scope"
		Just expect -> case unifyTypes expect valueType [] of
			Left msg -> flunk (expressionAt value) $ "illegal assignment: " ++ msg
			Right _ -> return ()
verifyStatementType scope (StatementDo value) = do
	_ <- getExpressionType scope value
	return ()
verifyStatementType scope (StatementIf _ condition body) = do
	conditionType <- getExpressionType scope condition
	assertTypeEqual (makeType "Bool") conditionType (expressionAt condition)
	_ <- verifyStatementBlock scope body
	return ()
verifyStatementType scope (StatementIfElse _ condition bodyThen bodyElse) = do
	conditionType <- getExpressionType scope condition
	assertTypeEqual (makeType "Bool") conditionType (expressionAt condition)
	_ <- verifyStatementBlock scope bodyThen
	_ <- verifyStatementBlock scope bodyElse
	return ()
verifyStatementType scope (StatementWhile _ condition body) = do
	conditionType <- getExpressionType scope condition
	assertTypeEqual (makeType "Bool") conditionType (expressionAt condition)
	_ <- verifyStatementBlock scope body
	return ()
verifyStatementType scope (StatementReturn returnToken Nothing) = case unifyTypes (mustReturn scope) (makeType "Void") [] of
	Left msg -> flunk returnToken $ "illegal return; expected type `" ++ niceType (mustReturn scope) ++ "`; " ++ msg 
	Right _ -> return ()
verifyStatementType scope (StatementReturn _returnToken (Just value)) = do
	valueType <- getExpressionType scope value
	case unifyTypes (mustReturn scope) valueType [] of
		Left msg -> flunk (expressionAt value) $ "illegal return value: " ++ msg
		Right _ -> return ()
verifyStatementType scope (StatementFunc _funcToken _ generics arguments bang returns body) = do
	verifyTypeScope scope funcType
	let scopeBody = setReturn returnType $ addVariables AssignFinal arguments $ declareTypes (map (\g -> (token g, [], [])) generics) $ scope
	_ <- verifyStatementBlock scopeBody body
	return ()
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
verifyStatementType _ StatementBreak{} = return ()
verifyStatementType scope (StatementLet _ body) = do
	mapM_ (verifyStatementType scope) body
	return ()
verifyStatementType _scope StatementStruct{} = return () -- TODO: kinds / concreteness?
verifyStatementBlock :: Scope -> [Statement] -> Check Scope
verifyStatementBlock scope [] = return scope
verifyStatementBlock scope (s : ss) = do
	scope' <- verifyStatementTypeDeclare scope s
	verifyStatementBlock scope' ss

topScope :: Scope
topScope =
	Scope (makeType "Void")
		[ ("Void", [], []), ("Int", [], []), ("String", [], []), ("Bool", [], []), ("Integer", [], []) ]
		[ (AssignFinal, Token "print" (FileEnd "^") Identifier, makeType "Int" `TypeArrow` TypeBangArrow (Token "!" (FileEnd "*") Special) (makeType "Void"))
		, (AssignFinal, Token "putStr" (FileEnd "^") Identifier, makeType "String" `TypeArrow` TypeBangArrow (Token "!" (FileEnd "*") Special) (makeType "Void"))
		, (AssignFinal, Token "not" (FileEnd "^") Identifier, makeType "Bool" `TypeArrow` makeType "Bool")
		, (AssignFinal, Token "show" (FileEnd "^") Identifier, makeType "Int" `TypeArrow` makeType "String")
		, (AssignFinal, Token "iAdd" (FileEnd "^") Identifier, binType (makeType "Integer"))
		, (AssignFinal, Token "iSubtract" (FileEnd "^") Identifier, binType (makeType "Integer"))
		, (AssignFinal, Token "iMultiply" (FileEnd "^") Identifier, binType (makeType "Integer"))
		, (AssignFinal, Token "iDivide" (FileEnd "^") Identifier, binType (makeType "Integer"))
		, (AssignFinal, Token "iMod" (FileEnd "^") Identifier, binType (makeType "Integer"))
		, (AssignFinal, Token "iNegate" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` makeType "Integer" )
		, (AssignFinal, Token "iPrint" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` TypeBangArrow (Token "!" (FileEnd "*") Special) (makeType "Void"))
		, (AssignFinal, Token "iLess" (FileEnd "^") Identifier, relation (makeType "Integer"))
		, (AssignFinal, Token "iEquals" (FileEnd "^") Identifier, relation (makeType "Integer"))
		, (AssignFinal, Token "big" (FileEnd "^") Identifier, makeType "Int" `TypeArrow` makeType "Integer")
		]
	where
	binType t = t `TypeArrow` (t `TypeArrow` t)
	relation t = t `TypeArrow` (t `TypeArrow` makeType "Bool")

verifyProgram :: Statement -> Check ()
verifyProgram program = verifyStatementTypeDeclare topScope program >> return ()
