
module Expression where

import Lex
import Parse
import ParseType

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
	||| parseFunc

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
	funcToken <- expectSpecial "func" "func begins function declaration"
	funcArgs <- parseManyUntil (peekTokenName ":" ||| peekTokenName "{" ||| peekTokenName "!") parseFuncArg
	bang <- maybeCheckTokenName "!"
	returnType <- parseMaybe (expectSpecial ":" "(optional) return type" >> parseType)
	body <- parseBlock
	return $ ExpressionFunc funcToken funcArgs bang returnType body

data Op a = OpLeaf a | OpBranch (Op a) Token (Op a) deriving Show

parseOp :: [String] -> Parse a -> Parse (Op a)
parseOp names atom = go where
	go = do
	left <- atom
	opMaybe <- lookOperators names
	case opMaybe of
		Nothing -> return $ OpLeaf left
		Just op -> do
			right <- go
			return $ OpBranch (OpLeaf left) op right

opReplace :: (a -> t) -> (t -> Token -> t -> t) -> Op a -> t
opReplace leafs branches tree = go tree where
	go (OpLeaf x) = leafs x
	go (OpBranch left' op right') = branches left op right where
		left = go left'
		right = go right'


opFlip :: Op a -> Op a
opFlip (OpLeaf a) = OpLeaf a
opFlip (OpBranch left op (OpBranch middle op' right)) = opFlip $ OpBranch (OpBranch left op middle) op' right
opFlip (OpBranch left op right) = OpBranch left op $ opFlip right

data OpAssociativity = OpLeft | OpRight | OpNone | OpPrefix


parseOperators :: [(OpAssociativity, [String])] -> Parse Expression
parseOperators [] = parseCall
parseOperators ((dir, ops) : rest) = case dir of
		OpNone -> parseOperators ((OpRight, ops) : rest) -- TODO: make it an error to mix these
		OpPrefix -> do
			token <- lookOperators ops
			case token of
				Nothing -> parseOperators rest
				Just t -> do
					operand <- parseOperators ((dir, ops) : rest)
					return $ ExpressionPrefix t operand
		_ -> return . opReplace id ExpressionOp =<< fmap (flipper dir) (parseOp ops (parseOperators rest)) where
	where
	flipper dir = case dir of
		OpRight -> id
		OpLeft -> opFlip

parseExpression :: Parse Expression
parseExpression = parseOperators
	[ (OpRight, ["$"])
	, (OpRight, [">>", ">>="])
	, (OpLeft, ["<<", "=<<"])
	, (OpLeft, ["."])
	, (OpRight, ["||"])
	, (OpRight, ["&&"])
	, (OpNone, ["==","<",">","<=",">=","/="])
	, (OpRight, ["++"])
	, (OpLeft, ["+", "-"])
	, (OpLeft, ["*", "/", "%"])
	, (OpPrefix, ["-"])
	]

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
	ifToken <- expectSpecial "if" "expected `if` to begin if-statement"
	condition <- parseExpression
	thenBlock <- parseBlock
	checkTokenName "else" ??? (do
		elseBlock <- parseBlock
		return $ StatementIfElse ifToken condition thenBlock elseBlock, return $ StatementIf ifToken condition thenBlock)

parseWhile :: Parse Statement
parseWhile = do
	whileToken <- expectSpecial "while" "expected `while` to begin while-statement"
	condition <- parseExpression
	body <- parseBlock
	return $ StatementWhile whileToken condition body

parseReturn :: Parse Statement
parseReturn = do
	returnToken <- expectSpecial "return" "expected `return` to begin return-statement"
	checkTokenName ";" ??? (return $ StatementReturn returnToken Nothing, do
		value <- parseExpression
		expectSpecial ";" "expected `;` to follow return statement expression"
		return $ StatementReturn returnToken $ Just value)

parseBreak :: Parse Statement
parseBreak = do
	breakToken <- expectSpecial "break" "expected `break` to begin break-statement"
	return $ StatementBreak breakToken

parseLet :: Parse Statement
parseLet = do
	letToken <- expectSpecial "let" "expected `let` to begin let-block"
	-- although we only want assignments & variable declarations,
	-- it's better for parsing to just consume a block (and report good error messages)
	block <- parseBlock
	return $ StatementLet letToken block

parseFuncDef :: Parse Statement
parseFuncDef = do
	funcToken <- expectSpecial "func" "expected `func` to begin function declaration"
	funcName <- expectIdentifier "expected function name to follow `func` keyword"
	funcArgs <- parseManyUntil (peekTokenName ":" ||| peekTokenName "{" ||| peekTokenName "!") parseFuncArg
	bang <- maybeCheckTokenName "!"
	returnType <- parseMaybe (expectSpecial ":" "(optional) return type" >> parseType)
	body <- parseBlock
	return $ StatementFunc funcToken funcName funcArgs bang returnType body

parseStatement :: Parse Statement
parseStatement = do
	peek <- peekMaybe
	case peek of
		Nothing -> reject "expected statement"
		Just t -> case token t of
			"var" -> parseVar
			"if" -> parseIf
			"while" -> parseWhile
			"return" -> parseReturn
			"let" -> parseLet
			"func" -> parseFuncDef
			_ -> parseAssign ||| parseDo

parseBlock :: Parse [Statement]
parseBlock = do
	expectSpecial "{" "expected `{` to open block"
	body <- parseManyUntil (peekTokenName "}") parseStatement
	expectSpecial "}" "expected `}` to close block"
	return body
