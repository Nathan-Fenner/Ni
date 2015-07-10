
module ParseExpression where

import Lex
import Parse
import ParseType
import ParseStatement

data Expression
	= ExpressionIdentifier Token
	| ExpressionIntegerLiteral Token
	| ExpressionDecimalLiteral Token
	| ExpressionStringLiteral Token
	| ExpressionBang Token
	| ExpressionCall Expression [Expression]
	| ExpressionFunc { arguments :: [(Token, Type)], funcBang :: Maybe Token, returnType :: Maybe Type , body :: [Statement] }
	deriving Show

parseIdentifier :: Parse Expression
parseIdentifier = fmap ExpressionIdentifier (expectIdentifier "name")

parseIntegerLiteral :: Parse Expression
parseIntegerLiteral = fmap ExpressionIntegerLiteral (expectToken (\t -> kind t == IntegerLiteral) "integer literal")

parseDecimalLiteral :: Parse Expression
parseDecimalLiteral = fmap ExpressionDecimalLiteral (expectToken (\t -> kind t == DecimalLiteral) "decimal literal")

parseStringLiteral :: Parse Expression
parseStringLiteral = fmap ExpressionStringLiteral (expectToken (\t -> kind t == StringLiteral) "string literal")

parseParens :: Parse Expression
parseParens = do
	expectSpecial "(" "open paren"
	e <- parseExpression
	expectSpecial ")" "expected `)` to match `(`"
	return e

parseAtom :: Parse Expression
parseAtom
	= parseIdentifier
	||| parseIntegerLiteral
	||| parseDecimalLiteral
	||| parseStringLiteral
	||| parseParens

parseBang :: Parse Expression
parseBang = fmap ExpressionBang $ expectSpecial "!" "bang"

parseCall :: Parse Expression
parseCall = do
	fun <- parseAtom
	args <- parseMany (parseBang ||| parseAtom)
	return $ case args of
		[] -> fun -- This cuts down on the number of non-calls everywhere
		_ -> ExpressionCall fun args

parseFuncArg :: Parse (Token, Type)
parseFuncArg = do
	expectSpecial "(" "to open argument"
	argName <- expectIdentifier "argument name"
	expectSpecial ":" "colon for type indicator"
	argType <- parseTypeAtom
	expectSpecial ")" "to close corresponding `(`"
	return (argName, argType)

parseFunc :: Parse Expression
parseFunc = do
	expectSpecial "func" "func begins function declaration"
	funcArgs <- parseManyUntil (peekTokenName ":" ||| peekTokenName "{" ||| peekTokenName "!") parseFuncArg
	bang <- checkTokenName "!"
	returnType <- parseMaybe (expectSpecial ":" "(optional) return type" >> parseType)
	body <- parseBlock
	return $ ExpressionFunc funcArgs bang returnType body

{-
parseFunc :: Parse Expression
parseFunc = do
	expectSpecial "func" "func"
	parseFuncArg 
-}
parseExpression :: Parse Expression
parseExpression = parseCall

