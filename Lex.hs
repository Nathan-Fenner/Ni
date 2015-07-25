   
module Lex where

data Location
	= Location
	{ file   :: FilePath
	, line   :: Int
	, column :: Int
	}
	| FileEnd FilePath
	deriving Show

pretty :: Location -> String
pretty (Location f l c) = f ++ "/" ++ show l ++ ":" ++ show c
pretty (FileEnd f) = f ++ "/end"

data Map from to = Map [(from, to)]

from :: Eq from => Map from to -> from -> Maybe to
from (Map t) n = case [y | (x, y) <- t, x == n] of
	[] -> Nothing
	(y : _) -> Just y


newline :: Location -> Location
newline (Location f l _) = Location f (l+1) 1
newline l = l

tab :: Location -> Location
tab (Location f l c) = Location f l (((c-1) `div` 4 + 1) * 4 + 1)
tab l = l

carriageReturn :: Location -> Location
carriageReturn (Location f l _) = (Location f l 1)
carriageReturn l = l

forward :: Int -> Location -> Location
forward n (Location f l c) = Location f l (c+n)
forward _ l = l

space :: Location -> Location
space = forward 1

data TokenKind
	= Identifier
	| Operator
	| Special
	| IntegerLiteral
	| DecimalLiteral
	| StringLiteral
	deriving (Show, Eq)

data Token = Token
	{ token :: String
	, at    :: Location
	, kind  :: TokenKind
	}
	deriving Show

lexer :: String -> String -> [Token]
lexer code filename = lex' code (Location filename 1 1)

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile f string = splitWhile' f [] string

splitWhile' :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
splitWhile' _ before [] = (before, [])
splitWhile' f before (a:after)
	|f a = splitWhile' f (before ++ [a]) after -- TODO: fix performance
	|otherwise =  (before, a:after)

lexString :: String -> String -> Location -> Location -> [Token]
lexString [] _ _ _ = error "reached end of file while parsing string literal"
lexString ('"' : rest) sofar start loc = Token sofar start StringLiteral : lex' rest (space loc)
lexString ('\n': _) _ _ _ = error "reached end of line while parsing string literal"
lexString ('\\':c:rest) sofar start loc = case from cMap c of
	Just r -> lexString rest (sofar ++ r) start (forward 2 loc)
	Nothing -> error "unknown escape character"
	where
	-- TODO: add more escaped characters
	cMap = Map [('\\', "\\"), ('n', "\n"), ('"', "\""), ('t', "\t"), ('v', "\v"), ('r', "\r"), ('0', "\0")]
lexString (c : rest) sofar start loc = lexString rest (sofar ++ [c]) start (space loc)

-- TODO: support exponential floating notation
lexNumber :: String -> Location -> [Token]
lexNumber ns loc = case rest of
	('.' : more) -> let
		(decimal, after) = splitWhile (`elem` ['0'..'9']) more
		literal = digits ++ "." ++ decimal
	 in
		Token literal loc DecimalLiteral : lex' after (forward (length literal) loc)
	_ -> Token digits loc IntegerLiteral : lex' rest (forward (length digits) loc)
	where
	(digits, rest) = splitWhile (`elem` ['0'..'9']) ns

lex' :: String -> Location -> [Token]
lex' "" _ = []
lex' (c:cs) loc
	-- TODO: comments
	|c == ' ' = lex' cs (space loc)
	-- TODO: treat whitespace better
	|c == '\r' = lex' cs (carriageReturn loc)
	|c == '\n' = lex' cs (newline loc)
	|c == '\t' = lex' cs (tab loc)
	|c `elem` ";{}()[].,!" = Token [c] loc Special : lex' cs (space loc)
	|c `elem` ops = let (before, after) = splitWhile (`elem` ops) (c:cs) in
		Token before loc (if before `elem` specialOperators then Special else Operator) : lex' after (forward (length before) loc)
	|c `elem` identStart = let (before, after) = splitWhile (`elem` identContinue) (c:cs) in
		Token before loc (wordType before) : lex' after (forward (length before) loc)
	|c == '"' = lexString cs "" loc loc
	|c `elem` ['0'..'9'] = lexNumber (c:cs) loc
	|otherwise = error $ "Invalid character `" ++ [c] ++ "` in source"
	where
	ops :: String
	ops = "+-*/%=<>#$^&|?:"
	specialOperators = ["->", "=", ":"]
	identStart :: String
	identStart = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
	identContinue :: String
	identContinue = identStart ++ ['0'..'9'] ++ "'"

wordType :: String -> TokenKind
wordType word
	|word `elem` specialWords = Special
	|otherwise = Identifier

specialWords :: [String]
specialWords = ["func", "while", "if", "else", "for", "var", "let", "struct", "return", "True", "False"]
