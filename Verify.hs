
module Verify(verifyAll, Verify) where

import Expression
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
