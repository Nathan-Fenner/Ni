
module ParseStatement where

import Lex
import Parse
import ParseType
import ParseExpression

data Statement
	= StatementAssign Token Expression
	| StatementVarVoid Token Type
	| StatementVarAssign Token Type Expression
	| StatementDo Expression
	| StatementIf Expression [Statement]
	| StatementIfElse Expression [Statement] [Statement]
	| StatementWhile Expression [Statement]
	| StatementReturn (Maybe Expression)
	| StatementBreak
	deriving Show

parseAssign :: Parse Statement
parseAssign = do
	name <- expectIdentifier "expected name to begin assignment"
	expectSpecial "=" "assignment operator `=` to follow variable name"
	right <- parseExpression
	expectSpecial ";" "semicolon should terminate statement"
	return $ StatementAssign name right

parseVar :: Parse Statement
parseVar = do
	expectSpecial "var" "expected `var` to begin variable initialization"
	name <- expectIdentifier "expected name to appear in assignment"
	expectSpecial ":" "expected `:` to declare variable type after `var`"
	varType <- parseType
	result <- checkTokenName "=" ??? ( parseExpression >>= return . StatementVarAssign name varType, return $ StatementVarVoid name varType)
	expectSpecial ";" "semicolon should terminate var declaration"
	return result

parseDo :: Parse Statement
parseDo = do
	expression <- parseExpression
	expectSpecial ";" "expected `;` to follow expression performance"
	return $ StatementDo expression

parseIf :: Parse Statement
parseIf = do
	expectSpecial "if" "expected `if` to begin if-statement"
	condition <- parseExpression
	thenBlock <- parseBlock
	checkTokenName "else" ??? (do
		elseBlock <- parseBlock
		return $ StatementIfElse condition thenBlock elseBlock, return $ StatementIf condition thenBlock)

parseWhile :: Parse Statement
parseWhile = do
	expectSpecial "while" "expected `while` to begin while-statement"
	condition <- parseExpression
	body <- parseBlock
	return $ StatementWhile condition body

parseReturn :: Parse Statement
parseReturn = do
	expectSpecial "return" "expected `return` to begin return-statement"
	checkTokenName ";" ??? (return $ StatementReturn Nothing, do
		value <- parseExpression
		expectSpecial ";" "expected `;` to follow return statement expression"
		return $ StatementReturn $ Just value)

parseStatement :: Parse Statement
parseStatement = parseVar ||| parseIf ||| parseWhile ||| parseReturn ||| parseAssign ||| parseDo

parseBlock :: Parse [Statement]
parseBlock = do
	expectSpecial "{" "expected `{` to open block"
	body <- parseManyUntil (peekTokenName "}") parseStatement
	expectSpecial "}" "expected `}` to close block"
	return body
