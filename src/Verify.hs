
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
data Marker = Mark | Protect deriving (Show, Eq)

data Scope = Scope
	{ scopeReturnType :: Type -- return type
	, scopeBangAllowed :: Bool -- whether banged expressions are allowable
	, scopeTypes :: [(String, [String], [(String, Type)])] -- type names in scope
	, scopeStack :: [(Token, Type)] -- stack & globals
	, assignStates :: [Either Marker (AssignState, Token)]
	, hasReturned :: Bool
	} deriving Show

getType :: Token -> Scope -> Maybe Type
getType name scope = case filter (\(n, _) -> token n == token name) (scopeStack scope) of
	[] -> Nothing
	((_, t):_) -> Just t

addVariable :: AssignState -> Token -> Type -> Scope -> Scope
addVariable assignState varName varType scope@Scope{scopeStack, assignStates} = scope{scopeStack = (varName, varType) : scopeStack, assignStates = Right (assignState, varName) : assignStates}

addVariables :: AssignState -> [(Token, Type)] -> Scope -> Scope
addVariables _ [] scope = scope
addVariables assignState ((n, t) : rest) scope = addVariables assignState rest $ addVariable assignState n t scope

addProtect :: Scope -> Scope
addProtect scope = scope{assignStates = Left Protect : assignStates scope}

getAssignState :: Token -> Scope -> AssignState
getAssignState name Scope{assignStates} = case filter filterer assignStates of
	[] -> error "!!! COMPILER ERROR: asked for assign state of undefined variable"
	(Right (s, _)  : _) -> s
	_ -> error "!!! COMPILER ERROR: asked for assign state and got a LEFT"
	where
	filterer (Right (_, v)) = token v == token name
	filterer _ = False

addLevel :: Scope -> Scope
addLevel scope = scope{assignStates = Left Mark : assignStates scope}

dropLevel :: Scope -> Scope
dropLevel scope@Scope{assignStates} = case assignStates of
	[] -> scope{assignStates = []}
	(Left Protect : rest) -> scope{assignStates = rest}
	(Left Mark : rest) -> scope{assignStates = rest}
	(_ : rest) -> dropLevel scope{assignStates = rest}

tryAssignment :: Token -> Scope -> Check Scope
tryAssignment var scope = do
	fixedUp <- fixUp (assignStates scope)
	return $ scope{ assignStates = fixedUp }
	where
	fixUp [] = error $ "tried to assign unknown variable " ++ show var
	fixUp (pair@(Right(assignState, varName)) : rest)
		|token varName == token var = case assignState of
			AssignFinal -> flunk var $ "variable `" ++ token var ++ "` is final and cannot be assigned to"
			Assigned -> return $ Right(Assigned, varName) : rest
			AssignNot -> return $ Right(Assigned, varName) : rest
		|otherwise = do
			rest' <- fixUp rest
			return $ pair : rest'
	fixUp (Left Protect : _) = flunk var $ "variable `" ++ token var ++ "` cannot be assigned; you cannot assign implicit parameters"
	fixUp (Left Mark : rest) = do
		rest' <- fixUp rest
		return $ Left Mark : rest'

declareType :: String -> [String] -> [(String, Type)] -> Scope -> Scope
declareType struct generics args scope@Scope{scopeTypes} = scope{scopeTypes = (struct, generics, args) : scopeTypes}

declareTypes :: [(String, [String], [(String, Type)])] -> Scope -> Scope
declareTypes [] scope = scope
declareTypes (p:ps) scope = declareTypes ps $ (\(x,y,z) -> declareType x y z) p scope

data Kind = KindConcrete | KindArrow Kind Kind deriving (Show, Eq)

checkTypeKind :: Scope -> Type -> Check Kind
checkTypeKind Scope{scopeTypes} (TypeName name) = go scopeTypes where
	go [] = flunk name $ "the type `" ++ token name ++ "` has not been declared"
	go ((name', generics, _) : rest)
		|token name == name' = return $ go' (length generics) 
		|otherwise = go rest
		where
		go' 0 = KindConcrete
		go' n = KindConcrete `KindArrow` go' (n-1)
checkTypeKind scope (TypeCall fun callArgs) = do
	funKind <- checkTypeKind scope fun
	match fun funKind callArgs
	where
	match :: Type -> Kind -> [Type] -> Check Kind
	match _ kind [] = return kind
	match sofar KindConcrete (arg:_) = flunk (typeAt arg) $ "applied type `" ++ niceType arg ++ "` to a concrete type `" ++ niceType sofar ++ "`"
	match sofar whole@(KindArrow left right) (arg:args) = do
		argKind <- checkTypeKind scope arg
		case left == argKind of
			True -> match (TypeCall sofar [arg]) right args
			False -> flunk (typeAt arg) $ "applied type `" ++ niceType arg ++ "` having kind " ++ show argKind ++ " to type `" ++ niceType sofar ++ "` having kind " ++ show whole ++ " (expecting argument to have kind " ++ show left ++ ")"
checkTypeKind scope (TypeArrow left right) = do
	checkTypeConcrete scope left
	checkTypeConcrete scope right
	return KindConcrete
checkTypeKind scope (TypeBangArrow _ right) = do
	checkTypeConcrete scope right
	return KindConcrete
checkTypeKind scope (TypeGenerics generics right) = checkTypeKind (declareTypes (map (\t -> (token t, [], [])) generics) scope) right

checkTypeConcrete :: Scope -> Type -> Check ()
checkTypeConcrete scope myType = do
	kind <- checkTypeKind scope myType
	case kind of
		KindConcrete -> return ()
		_ -> flunk (typeAt myType) $ "expected type `" ++ niceType myType ++ "` to be concrete, but has kind " ++ show kind

isTypeDeclared :: String -> Scope -> Bool
isTypeDeclared n Scope{scopeTypes} = n `elem` map (\(n',_,_) -> n') scopeTypes

lookupTypeDeclaration :: Type -> Scope -> Either String [(String, Type)]
lookupTypeDeclaration structType Scope{scopeTypes} = case canonical of
	Left _ -> Left $ "The type `" ++ niceType structType ++ "` is not a struct-type"
	Right (CanonicalType name generics) -> case [ (generics', fields') | (name', generics', fields') <- scopeTypes, name == name' ] of
		[] -> Left $ "There is no type with name `" ++ name ++ "`"
		((generics', fields'):_) -> case length generics' == length generics of
			False -> Left $ "struct type `" ++ name ++ "` expects " ++ show (length generics') ++ " type arguments, but it has been given " ++ show (length generics)
			True -> Right $ map (\(f, t) -> (,) f $ replaceTypes (zip generics' generics) t) fields'
	where
	canonical = canonicalType structType

mustReturn :: Scope -> Type
mustReturn Scope{scopeReturnType} = scopeReturnType

setReturn :: Type -> Scope -> Scope
setReturn r scope = scope{scopeReturnType = r}

assertTypeEqual :: Type -> Type -> Token -> Check ()
assertTypeEqual left right atToken
	|left === right = Pass ()
	|otherwise = flunk atToken $ "got type `" ++ niceType left ++ "` but expected type `" ++ niceType right ++ "`"

getExpressionType :: Scope -> Expression -> Check Type
getExpressionType scope (ExpressionIdentifier name) = case getType name scope of
	Nothing -> flunk name $ "variable `" ++ token name ++ "` has not been declared or is not in scope"
	Just t -> case getAssignState name scope of
		Assigned -> Pass t
		AssignFinal -> Pass t
		AssignNot -> flunk name $ "variable `" ++ token name ++ "` cannot be read because it might not have been initialized"
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
	match' (TypeBangArrow _ right) (ExpressionBang bangToken : rest) free sofar
		|scopeBangAllowed scope = match' right rest free sofar
		|otherwise = flunk bangToken $ "a bang `!` cannot appear in a pure function or a let block"
	match' (TypeBangArrow _ _) (arg : _) _ _ = flunk (expressionAt arg) $ "expected a `!` instead of value applied to object of function type"
	match' (TypeGenerics generics right) args@(arg:_) free sofar = case filter (`elem` free) (map token generics) of
		[] -> match' right args (map token generics ++ free) sofar
		shadowed -> flunk (expressionAt arg) $ "generics may not shadow each other (generic variable(s) " ++ intercalate ", " shadowed ++ ")"	
	match' fun (arg : _) _ _ = flunk (expressionAt arg) $ "cannot apply argument to object with non-function type `" ++ niceType fun ++ "`"

getExpressionType _scope (ExpressionBang bang) = flunk bang "a bang `!` outside of a matching function call is not allowed"
getExpressionType scope (ExpressionFunc funcToken generics args bang returns body) = do
	let scopeBody = setReturn returnType' $ addVariables AssignFinal args scope
	finalScopeBody <- verifyStatementBlock (addProtect scopeBody{scopeBangAllowed = allowsBang}) body
	case returnType' === makeType "Void" || hasReturned finalScopeBody of
		True -> return funcType
		False -> flunk funcToken $ "function is not Void but fails to unconditionally return"
	where
	returnType' = case returns of
		Nothing -> makeType "Void"
		Just t -> t
	allowsBang = case bang of
		Nothing -> False
		Just _  -> True
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
	verifyStatementType (addProtect newScope) statement
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
	mapM_ (verifyStatementType newScope{scopeBangAllowed = False}) body -- bangs cannot appear in a let-body
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
verifyStatementTypeDeclare scope statement@StatementReturn{} = do
	verifyStatementType scope statement
	return scope{hasReturned = True}
verifyStatementTypeDeclare scope statement@StatementIf{} = do
	-- TODO: check whether this is required
	verifyStatementType (addLevel scope) statement -- we don't strictly need this (yet)
	return scope
verifyStatementTypeDeclare scope statement@StatementWhile{} = do
	-- TODO: check whether this is required
	verifyStatementType (addLevel scope) statement -- we don't strictly need this (yet)
	return scope
verifyStatementTypeDeclare scope (StatementIfElse _ condition bodyThen bodyElse) = do
	conditionType <- getExpressionType scope condition
	assertTypeEqual (makeType "Bool") conditionType (expressionAt condition)
	thenScope <- verifyStatementBlock (addLevel scope) bodyThen
	elseScope <- verifyStatementBlock (addLevel scope) bodyElse
	let scope' = scope{hasReturned = hasReturned thenScope && hasReturned elseScope}
	return $ fixScope scope' thenScope elseScope
	where
	fixScope scope' thenScope elseScope = scope'{ assignStates = map together $ zip assignThen assignElse }
		where
		thenScope' = dropLevel thenScope
		elseScope' = dropLevel elseScope
		assignThen = through thenScope'
		assignElse = through elseScope'
		together (Right (sA, v), Right (sB, v'))
			|token v /= token v' = error $ "!!! COMPILER ERROR: matched `" ++ show v ++ "` with `" ++ show v' ++ "`"
			|otherwise = Right (sA `also` sB , v)
		together (Left m, Left m')
			|m == m' = Left m
			|otherwise = error $ "!!! COMPILER ERROR: matched incompatible " ++ show m ++ " with " ++ show m'
		together (x, y) = error $ "!!! COMPILER ERROR: matched incompatible " ++ show x ++ " with " ++ show y
		also Assigned Assigned = Assigned
		also AssignNot Assigned = AssignNot
		also Assigned AssignNot = AssignNot
		also x y
			|x == y = x
			|otherwise = error $ "!!! COMPILER ERROR: matched incompatible " ++ show x ++ " and " ++ show y
		through s = case hasReturned s of
			True -> map allAssigned $ assignStates s
			False -> assignStates s
		allAssigned (Right (AssignNot, v)) = Right (Assigned, v)
		allAssigned x = x

-- The catch-all for the rest:
verifyStatementTypeDeclare scope statement = verifyStatementType scope statement >> return scope

verifyStatementType :: Scope -> Statement -> Check ()
verifyStatementType scope (StatementVarVoid _ varType) = do
	checkTypeConcrete scope varType
	return ()
verifyStatementType scope (StatementVarAssign _ varType value) = do
	checkTypeConcrete scope varType
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
verifyStatementType scope (StatementFunc _funcToken funcName generics arguments bang returns body) = do
	checkTypeConcrete scope funcType
	let scopeBody = setReturn returnType' $ addVariables AssignFinal arguments $ declareTypes (map (\g -> (token g, [], [])) generics) $ scope
	finalBodyScope <- verifyStatementBlock scopeBody{scopeBangAllowed = allowsBang} body
	case returnType' === makeType "Void" || hasReturned finalBodyScope of
		True -> return () -- doesn't require that we've reached a return
		False -> flunk funcName $ "function `" ++ token funcName ++ "` isn't a Void function, but fails to return unconditionally"
	where
	returnType' = case returns of
		Nothing -> makeType "Void"
		Just t -> t
	allowsBang = case bang of
		Nothing -> False
		Just _  -> True
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
verifyStatementBlock scope (s : ss)
	|hasReturned scope = flunk (statementAt s) $ "statement is unreachable"
	|otherwise = do
		scope' <- verifyStatementTypeDeclare scope s
		verifyStatementBlock scope' ss

topScope :: Scope
topScope =
	Scope
		(makeType "Void")
		False
		[ ("Void", [], []), ("Int", [], []), ("String", [], []), ("Bool", [], []), ("Integer", [], []) ]
		[ (Token "print" (FileEnd "^") Identifier, makeType "Int" `TypeArrow` TypeBangArrow (Token "!" (FileEnd "*") Special) (makeType "Void"))
		, (Token "putStr" (FileEnd "^") Identifier, makeType "String" `TypeArrow` TypeBangArrow (Token "!" (FileEnd "*") Special) (makeType "Void"))
		, (Token "not" (FileEnd "^") Identifier, makeType "Bool" `TypeArrow` makeType "Bool")
		, (Token "show" (FileEnd "^") Identifier, makeType "Int" `TypeArrow` makeType "String")
		, (Token "iAdd" (FileEnd "^") Identifier, binType (makeType "Integer"))
		, (Token "iSubtract" (FileEnd "^") Identifier, binType (makeType "Integer"))
		, (Token "iMultiply" (FileEnd "^") Identifier, binType (makeType "Integer"))
		, (Token "iDivide" (FileEnd "^") Identifier, binType (makeType "Integer"))
		, (Token "iMod" (FileEnd "^") Identifier, binType (makeType "Integer"))
		, (Token "iNegate" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` makeType "Integer" )
		, (Token "iPrint" (FileEnd "^") Identifier, makeType "Integer" `TypeArrow` TypeBangArrow (Token "!" (FileEnd "*") Special) (makeType "Void"))
		, (Token "iLess" (FileEnd "^") Identifier, relation (makeType "Integer"))
		, (Token "iEquals" (FileEnd "^") Identifier, relation (makeType "Integer"))
		, (Token "big" (FileEnd "^") Identifier, makeType "Int" `TypeArrow` makeType "Integer")
		]
		(map foreignSet
			[  "print"
			,  "putStr"
			,  "not"
			,  "show"
			,  "iAdd"
			, "iSubtract"
			, "iMultiply"
			,  "iDivide"
			,  "iMod"
			,  "iNegate"
			,  "iPrint"
			,  "iLess"
			,  "iEquals"
			,  "big"
			]
		)
		False
	where
	binType t = t `TypeArrow` (t `TypeArrow` t)
	relation t = t `TypeArrow` (t `TypeArrow` makeType "Bool")
	foreignToken name = Token name (FileEnd "^") Identifier
	foreignSet name = Right (AssignFinal, foreignToken name)

verifyProgram :: Statement -> Check ()
verifyProgram program = verifyStatementTypeDeclare topScope program >> return ()
