
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

{-
data Statement
	| StatementLet Token [Statement]
	deriving Show
-}

x &&&>>> (a, b) = (x &&& a, b)
(x, _) .&&&>>> (a, b) = (x &&& a, b)

verifyExpressionTypeIs :: Scope -> Expression -> Type -> Verify

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
verifyStatementType scope (StatementLet _ body) = error "unimplemented let-statements"
verifyStatementType scope statement = error $ "Statement type not implemented: " ++ show statement

verifyBlockType :: Scope -> [Statement] -> (Verify, Scope)
verifyBlockType scope (statement : rest) = let (state, scope') = verifyStatementType scope statement in state &&&>>> verifyBlockType scope' rest
verifyBlockType scope [] = (Success, scope)

