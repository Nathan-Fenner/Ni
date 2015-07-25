
module Expression where

import Lex
import Parse
import ParseType

data Expression
	= ExpressionIdentifier Token
	| ExpressionIntegerLiteral Token
	| ExpressionDecimalLiteral Token
	| ExpressionStringLiteral Token
	| ExpressionBoolLiteral Token
	| ExpressionBang Token
	| ExpressionCall Expression [Expression]
	| ExpressionDot Expression Token
	| ExpressionFunc
		{ anonFuncToken :: Token
		, arguments :: [(Token, Type)]
		, funcBang :: Maybe Token
		, returnType :: Maybe Type
		, body :: [Statement]
		}
	| ExpressionOp Expression Token Expression
	| ExpressionPrefix Token Expression
	| ExpressionConstructor Token [(Token, Expression)]
	deriving Show

parseIdentifier :: Parse Expression
parseIdentifier = fmap ExpressionIdentifier (expectIdentifier "name")

parseIntegerLiteral :: Parse Expression
parseIntegerLiteral = fmap ExpressionIntegerLiteral (expectToken (\t -> kind t == IntegerLiteral) "integer literal")

parseDecimalLiteral :: Parse Expression
parseDecimalLiteral = fmap ExpressionDecimalLiteral (expectToken (\t -> kind t == DecimalLiteral) "decimal literal")

parseStringLiteral :: Parse Expression
parseStringLiteral = fmap ExpressionStringLiteral (expectToken (\t -> kind t == StringLiteral) "string literal")

parseBoolLiteral :: Parse Expression
parseBoolLiteral = fmap ExpressionBoolLiteral $ (expectSpecial "true" "true") ||| (expectSpecial "false" "false")

parseParens :: Parse Expression
parseParens = do
	_ <- expectSpecial "(" "open paren"
	e <- parseExpression
	_ <- expectSpecial ")" "expected `)` to match `(`" -- TODO: add location of matching paren
	return e

parseConstructor :: Parse Expression
parseConstructor = do
	name <- expectIdentifier "name"
	isConstructor <- peekTokenName "{"
	if isConstructor then do
		_ <- expectSpecial "{" "expected `{` to open constructor arguments"
		block <- interior
		_ <- expectSpecial "}" "expected `}` to close constructor arguments"
		return $ ExpressionConstructor name block
		else do
			return $ ExpressionIdentifier name
	where
	interior = do -- TODO: allow empty constructors
		first <- element
		rest <- parseManyUntil (peekTokenName "}") (expectSpecial "," "expected `,` to separate field entries" >> element)
		return $ first : rest
	element :: Parse (Token, Expression)
	element = do
		name <- expectIdentifier "expected identifier to name field"
		_ <- expectSpecial "=" $ "expected `=` to follow field `" ++ token name ++ "` in constructor"
		value <- parseExpression
		return (name, value)

parseAtom :: Parse Expression
parseAtom
	= parseConstructor
	-- ||| parseIdentifier
	||| parseIntegerLiteral
	||| parseDecimalLiteral
	||| parseStringLiteral
	||| parseBoolLiteral
	||| parseParens
	||| parseFunc
	||| reject "expected an expression here; maybe you forgot a semicolon?"

parseDot :: Parse Expression
parseDot = do
	atom <- parseAtom
	dots <- parseManyUntil (fmap not $ peekTokenName ".") (expectSpecial "." "dots begin accesses" >> expectIdentifier "an identifier must follow a member access with `.`")
	return $ wrapUp atom dots
	where
	wrapUp atom [] = atom
	wrapUp atom (name : rest) = wrapUp (ExpressionDot atom name) rest

parseBang :: Parse Expression
parseBang = fmap ExpressionBang $ expectSpecial "!" "bang"

parseCall :: Parse Expression
parseCall = do
	fun <- parseDot
	args <- parseMany (parseBang ||| parseDot)
	return $ case args of
		[] -> fun -- This cuts down on the number of non-calls everywhere
		_ -> ExpressionCall fun args

parseFuncArg :: Parse (Token, Type)
parseFuncArg = do
	_ <- expectSpecial "(" "expected `(` to open argument"
	argName <- expectIdentifier "argument name"
	_ <- expectSpecial ":" "expected `:` for type indicator"
	argType <- parseType
	_ <- expectSpecial ")" "expected `)` to close corresponding `(`" -- TODO: location of matching paren
	return (argName, argType)

parseFunc :: Parse Expression
parseFunc = do
	funcWord <- expectSpecial "func" "func begins function declaration"
	funcArgs <- parseManyUntil (peekTokenName ":" ^|| peekTokenName "{" ^|| peekTokenName "!") parseFuncArg
	bang <- maybeCheckTokenName "!"
	returns <- parseMaybe (expectSpecial ":" "(optional) return type" >> parseType)
	block <- parseBlock
	return $ ExpressionFunc funcWord funcArgs bang returns block

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
			opToken <- lookOperators ops
			case opToken of
				Nothing -> parseOperators rest
				Just t -> do
					operand <- parseOperators ((dir, ops) : rest)
					return $ ExpressionPrefix t operand
		OpRight -> process id
		OpLeft -> process opFlip
	where
	process flipper = return . opReplace id ExpressionOp =<< fmap flipper (parseOp ops (parseOperators rest))

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
	| StatementStruct Token Token [(Token, Type)]
	deriving Show

parseAssign :: Parse Statement
parseAssign = do
	name <- expectIdentifier "expected name to begin assignment"
	_ <- expectSpecial "=" "expected assignment operator `=` to follow variable name"
	right <- parseExpression
	_ <- expectSpecial ";" "expected `;` to terminate statement"
	return $ StatementAssign name right

parseVar :: Parse Statement
parseVar = do
	_ <- expectSpecial "var" "expected `var` to begin variable initialization"
	name <- expectIdentifier "expected name to appear in assignment"
	_ <- expectSpecial ":" "expected `:` to declare variable type after `var`"
	varType <- parseType
	result <- checkTokenName "=" ??? ( parseExpression >>= return . StatementVarAssign name varType, return $ StatementVarVoid name varType)
	_ <- expectSpecial ";" "expected `;` to terminate var declaration"
	return result

parseDo :: Parse Statement
parseDo = do
	expression <- parseExpression
	_ <- expectSpecial ";" "expected `;` to follow expression performance"
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
	block <- parseBlock
	return $ StatementWhile whileToken condition block

parseReturn :: Parse Statement
parseReturn = do
	returnToken <- expectSpecial "return" "expected `return` to begin return-statement"
	checkTokenName ";" ??? (return $ StatementReturn returnToken Nothing, do
		value <- parseExpression
		_ <- expectSpecial ";" "expected `;` to follow return statement expression"
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

parseStruct :: Parse Statement
parseStruct = do
	structToken <- expectSpecial "struct" "expected `struct` to begin struct definition"
	name <- expectIdentifier "expected struct name to follow `struct` token"
	block <- structBody
	return $ StatementStruct structToken name block
	where
	structBody = do
		_ <- expectSpecial "{" "expected `{` to open struct definition block"
		block <- parseManyUntil (peekTokenName "}") structMember
		_ <- expectSpecial "}" "expected `}` to close struct definition block" -- TODO: matching
		return block
	structMember = do
		name <- expectIdentifier "expected field name to appear in struct definition block"
		_ <- expectSpecial ":" "expected `:` to follow field name to define field type"
		t <- parseType ||| reject ("expected type to follow field name `" ++ token name ++ "` in struct definition")
		_ <- expectSpecial ";" "expected `;` to end field definition in struct"
		return (name, t)

(^||) :: Monad m => m Bool -> m Bool -> m Bool
x ^|| y = do
	x' <- x
	y' <- y
	return $ x' || y'

parseFuncDef :: Parse Statement
parseFuncDef = do
	funcWord <- expectSpecial "func" "expected `func` to begin function declaration"
	name <- expectIdentifier "expected function name to follow `func` keyword"
	funcArgs <- parseManyUntil (peekTokenName ":" ^|| peekTokenName "{" ^|| peekTokenName "!") parseFuncArg
	bang <- maybeCheckTokenName "!"
	returns <- parseMaybe (expectSpecial ":" "(optional) return type" >> parseType)
	block <- parseBlock
	return $ StatementFunc funcWord name funcArgs bang returns block

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
			"struct" -> parseStruct
			_ -> parseAssign ||| parseDo

parseBlock :: Parse [Statement]
parseBlock = do
	_ <- expectSpecial "{" "expected `{` to open block"
	block <- parseManyUntil (peekTokenName "}") parseStatement
	_ <- expectSpecial "}" "expected `}` to close block" -- TODO: add information about matching paren
	return block

parseModule :: Parse Statement
parseModule = do
	block <- parseManyUntil peekFileEnd parseStatement
	return $ StatementLet (Token "module" (FileEnd "^") Special) block