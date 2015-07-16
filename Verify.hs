
module Verify(verifyAll, Verify(Success, Failure)) where

import Expression
import ParseType
import Lex

data Verify = Success | Failure [(Token, String)]

(&&&) :: Verify -> Verify -> Verify
Success &&& msg = msg
msg &&& Success = msg
Failure xs &&& Failure ys = Failure (xs ++ ys)

(.&&&.) :: (Expression -> Verify) -> (Expression -> Verify) -> (Expression -> Verify)
(f .&&&. g) x = f x &&& g x

map_ :: (a -> Verify) -> [a] -> Verify
map_ f xs = go xs where
	go [] = Success
	go (x:xs) = f x &&& go xs


{-
data Statement
	= StatementAssign Token Expression
	| StatementVarVoid Token Type
	| StatementVarAssign Token Type Expression
	| StatementDo Expression
	| StatementIf Token Expression [Statement]
	| StatementIfElse Token Expression [Statement] [Statement]
	| StatementWhile Token Expression [Statement]
	| StatementReturn Token (Maybe Expression)
	| StatementBreak Token
	| StatementLet Token [Statement]
	| StatementFunc { funcToken :: Token, funcName :: Token, argumentsStatement :: [(Token, Type)], funcBangStatement :: Maybe Token, returnTypeStatement :: Maybe Type , bodyStatement :: [Statement] }
	deriving Show
-}

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


{-
data Expression
	= ExpressionIdentifier Token
	| ExpressionIntegerLiteral Token
	| ExpressionDecimalLiteral Token
	| ExpressionStringLiteral Token
	| ExpressionBang Token
	| ExpressionCall Expression [Expression]
	| ExpressionFunc { anonFuncToken :: Token, arguments :: [(Token, Type)], funcBang :: Maybe Token, returnType :: Maybe Type , body :: [Statement] }
	| ExpressionOp Expression Token Expression
	| ExpressionPrefix Token Expression
	deriving Show
-}


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

levels :: (Expression -> Verify) -> (Statement -> Verify) -> Either Expression Statement -> Verify
levels f g x = (case x of Left x' -> goe x'; Right x' -> gos x') where
	goe :: Expression -> Verify
	goe x = f x &&& goe' x
	gos :: Statement -> Verify
	gos x = g x &&& gos' x
	goe' :: Expression -> Verify
	goe' (ExpressionCall fun args) = goe fun &&& map_ goe args
	goe' ExpressionFunc{} = Success -- TODO: body
	goe' (ExpressionOp left _ right) = goe left &&& goe right
	goe' (ExpressionPrefix _ arg) = goe arg
	goe' _ = Success
	gos' :: Statement -> Verify
	gos' (StatementAssign _ v) = goe v
	gos' (StatementVarAssign _ _ v) = goe v
	gos' (StatementDo e) = goe e
	gos' (StatementIf _ c body) = goe c &&& map_ gos body
	gos' (StatementIfElse _ c b1 b2) = goe c &&& map_ gos b1 &&& map_ gos b2
	gos' (StatementWhile _ c b) = goe c &&& map_ gos b
	gos' (StatementReturn _ (Just e)) = goe e
	gos' (StatementLet _ b) = map_ gos b
	gos' (StatementFunc{bodyStatement = b}) = map_ gos b
	gos' _ = Success

letVerifier :: Statement -> Verify
letVerifier (StatementLet _ body) = map_ verifyKind body where
	verifyKind StatementVarAssign{} = Success
	verifyKind StatementAssign{} = Success
	verifyKind StatementFunc{} = Success
	verifyKind s = Failure [(statementAt s, "let-blocks may contain only assignments, variable declarations with assignment, or function definitions.")]
letVerifier _ = Success -- skip over all non-let statements

verifyAll :: Either Expression Statement -> Verify
verifyAll = levels (const Success) letVerifier

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

setReturn :: Scope -> Type -> Scope
setReturn (Scope _ ts) r = Scope r ts

x &&&>>> (a, b) = (x &&& a, b)
(x, _) .&&&>>> (a, b) = (x &&& a, b)

{-
data Expression
	| ExpressionBang Token ???
	| ExpressionCall Expression [Expression]
	| ExpressionFunc { anonFuncToken :: Token, arguments :: [(Token, Type)], funcBang :: Maybe Token, returnType :: Maybe Type , body :: [Statement] }
	| ExpressionOp Expression Token Expression
	| ExpressionPrefix Token Expression
	deriving Show
-}


getExpressionType :: Scope -> Expression -> Either Verify Type
getExpressionType scope (ExpressionIdentifier name) = case getType name scope of
	Nothing -> Left $ Failure [(name, "variable `" ++ token name ++ "` has not been declared or is not in scope")]
	Just t -> Right t
getExpressionType scope (ExpressionIntegerLiteral{}) = Right $ makeType "Int"
getExpressionType scope (ExpressionDecimalLiteral{}) = Right $ makeType "Float"
getExpressionType scope (ExpressionStringLiteral{}) = Right $ makeType "String"
getExpressionType scope (ExpressionCall fun args) = case funType of
	Left f -> Left f
	Right funType' -> matchFuncType funType' argTypes
	where
	funType :: Either Verify Type
	funType = getExpressionType scope fun
	argTypes :: [(Expression, Either Verify Type)]
	argTypes = zip args $ map (getExpressionType scope) args
	matchFuncType :: Type -> [(Expression, Either Verify Type)] -> Either Verify Type
	matchFuncType t [] = Right $ t
	matchFuncType (TypeArrow left right) ((arg, argType) : rest) = case argType of
		Left f -> Left f
		Right argType' -> case argType' === left of
			True -> matchFuncType right rest
			False -> Left $ Failure [(expressionAt arg, "expected type " ++ show left ++ " but expression " ++ show arg ++ " with type " ++ show argType' ++ " was applied instead")]

verifyExpressionTypeIs :: Scope -> Expression -> Type -> Verify
verifyExpressionTypeIs scope (ExpressionIdentifier name) expected = case getType name scope of
	Nothing -> Failure [(name, "variable `" ++ token name ++ "` has not been declared or is not in scope")]
	Just t -> if t === expected then Success else Failure [(name, "variable `" ++ token name ++ "` has type " ++ show t ++ " but type " ++ show expected ++ " was expected")]
verifyExpressionTypeIs scope (ExpressionIntegerLiteral int) expected = case expected === makeType "Int" of
	True -> Success
	False -> Failure [(int, "integer `" ++ show int ++ "` has type Int but " ++ show expected ++ " was expected")]
verifyExpressionTypeIs scope (ExpressionDecimalLiteral float) expected = case expected === makeType "Float" of
	True -> Success
	False -> Failure [(float, "float `" ++ show float ++ "` has type Float but " ++ show expected ++ " was expected")]
verifyExpressionTypeIs scope (ExpressionStringLiteral string) expected = case expected === makeType "String" of
	True -> Success
	False -> Failure [(string, "string `" ++ show string ++ "` has type String but " ++ show expected ++ " was expected")]
verifyExpressionTypeIs scope (ExpressionBang bang) expected = Failure [(bang, "found bang `!` where type " ++ show expected ++ " was expected")]
verifyExpressionTypeIs scope (ExpressionCall left []) expected = verifyExpressionTypeIs scope left expected

verifyExpressionTypeIs scope expression expected = error "unimplemented expression type"

verifyExpressionTypeConsistent :: Scope -> Expression -> Verify
verifyExpressionTypeConsistent scope expression = error "unimplemented expression type (consistency)"

verifyStatementType :: Scope -> Statement -> (Verify, Scope)
verifyStatementType scope (StatementVarVoid nameToken varType) = (Success, addType nameToken varType scope)
verifyStatementType scope (StatementAssign nameToken value) = (case getType nameToken scope of
	Nothing -> Failure $ [(nameToken, "variable has not been declared or is not in scope")]
	Just t -> verifyExpressionTypeIs scope value t, scope)
verifyStatementType scope (StatementVarAssign nameToken varType value) = (verifyExpressionTypeIs scope value varType, addType nameToken varType scope)
verifyStatementType scope (StatementDo expression) = (verifyExpressionTypeConsistent scope expression, scope)
verifyStatementType scope (StatementIf _ condition body)
	= verifyExpressionTypeIs scope condition (TypeName $ Token "Bool" (FileEnd "*") Identifier)
	&&&>>> verifyBlockType scope body
verifyStatementType scope (StatementIfElse _ condition body elseBody)
	= verifyExpressionTypeIs scope condition (TypeName $ Token "Bool" (FileEnd "*") Identifier)
	&&&>>> verifyBlockType scope body
	.&&&>>> verifyBlockType scope elseBody
verifyStatementType scope (StatementWhile _ condition body)
	= verifyExpressionTypeIs scope condition (TypeName $ Token "Bool" (FileEnd "*") Identifier)
	&&&>>> verifyBlockType scope body
verifyStatementType scope (StatementReturn returnToken Nothing) = case mustReturn scope of
	TypeName (Token "Void" _ _) -> (Success, scope)
	t -> (Failure [(returnToken, "expected to return type " ++ show (mustReturn scope) ++ " but returned nothing")], scope)
verifyStatementType scope (StatementReturn returnToken (Just expected)) = 
	(verifyExpressionTypeIs scope expected (mustReturn scope), scope)
verifyStatementType scope (StatementFunc funcToken funcName arguments bang returns body) = (checked, newScope)
	where
	(checked, _) = verifyBlockType (addTypes arguments $ setReturn newScope inferReturns) body
	newScope = addType funcName funcType scope
	inferReturns = case returns of Nothing -> TypeName (Token "Void" (FileEnd "*") Identifier); Just t -> t
	funcType = funcType' (map snd arguments ++ [inferReturns])
	funcType' [t] = t
	funcType' (t:ts) = TypeArrow t (funcType' ts)
verifyStatementType scope (StatementBreak _) = (Success, scope)
verifyStatementType scope (StatementLet _ body) = verifyBlockType (addTypes newVars scope) body
	where
	newVars = onlyJust $ map varForm body
	varForm (StatementVarVoid varName varType) = Just (varName, varType)
	varForm (StatementVarAssign varName varType _) = Just (varName, varType)
	varForm _ = Nothing
	onlyJust [] = []
	onlyJust (Just x : xs) = x : onlyJust xs
	onlyJust (_ : xs) = onlyJust xs
-- verifyStatementType scope statement = error $ "Statement type not implemented: " ++ show statement

verifyBlockType :: Scope -> [Statement] -> (Verify, Scope)
verifyBlockType scope (statement : rest) = let (state, scope') = verifyStatementType scope statement in state &&&>>> verifyBlockType scope' rest
verifyBlockType scope [] = (Success, scope)

