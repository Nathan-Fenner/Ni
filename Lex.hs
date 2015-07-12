   
module Lex where

import System.IO

data Location
	= Location
	{ file   :: FilePath
	, line   :: Int
	, column :: Int
	}
	| FileEnd FilePath
	deriving Show

data Map from to = Map [(from, to)]

from :: Eq from => Map from to -> from -> Maybe to
from (Map t) n = case [y | (x, y) <- t, x == n] of
	[] -> Nothing
	(y : _) -> Just y


newline :: Location -> Location
newline (Location f l _) = Location f (l+1) 1

tab :: Location -> Location
tab (Location f l c) = Location f l (((c-1) `div` 4 + 1) * 4 + 1)

carriageReturn :: Location -> Location
carriageReturn (Location f l _) = (Location f l 1)

forward :: Int -> Location -> Location
forward n (Location f l c) = Location f l (c+n)

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
lexer code file = lex' code (Location file 1 1)

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile f string = splitWhile' f [] string

splitWhile' :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
splitWhile' f before [] = (before, [])
splitWhile' f before (a:after)
	|f a = splitWhile' f (before ++ [a]) after -- TODO: fix performance
	|otherwise =  (before, a:after)

lexString :: String -> String -> Location -> Location -> [Token]
lexString [] _ _ _ = error "reached end of file while parsing string literal"
lexString ('"' : rest) sofar start at = Token sofar start StringLiteral : lex' rest (space at)
lexString ('\n': _) _ _ _ = error "reached end of line while parsing string literal"
lexString ('\\':c:rest) sofar start at = case from cMap c of
	Just r -> lexString rest (sofar ++ r) start (forward 2 at)
	Nothing -> error "unknown escape character"
	where
	-- TODO: add more escaped characters
	cMap = Map [('\\', "\\"), ('n', "\n"), ('"', "\""), ('t', "\t"), ('v', "\v"), ('r', "\r"), ('0', "\0")]
lexString (c : rest) sofar start at = lexString rest (sofar ++ [c]) start (space at)

-- TODO: support exponential floating notation
lexNumber :: String -> Location -> [Token]
lexNumber ns at = case rest of
	('.' : more) -> let
		(decimal, rest) = splitWhile (`elem` ['0'..'9']) more
		literal = digits ++ "." ++ rest
	 in
		Token literal at DecimalLiteral : lex' more (forward (length literal) at)
	_ -> Token digits at IntegerLiteral : lex' rest (forward (length digits) at)
	where
	(digits, rest) = splitWhile (`elem` ['0'..'9']) ns

lex' :: String -> Location -> [Token]
lex' "" _ = []
lex' (c:cs) at
	-- TODO: comments
	|c == ' ' = lex' cs (space at)
	-- TODO: treat whitespace better
	|c == '\r' = lex' cs (carriageReturn at)
	|c == '\n' = lex' cs (newline at)
	|c == '\t' = lex' cs (tab at)
	|c `elem` ";{}()[].!" = Token [c] at Special : lex' cs (space at)
	|c `elem` ops = let (before, after) = splitWhile (`elem` ops) (c:cs) in
		Token before at (if before `elem` specialOperators then Special else Operator) : lex' after (forward (length before) at)
	|c `elem` identStart = let (before, after) = splitWhile (`elem` identContinue) (c:cs) in
		Token before at (wordType before) : lex' after (forward (length before) at)
	|c == '"' = lexString cs "" at at
	|c `elem` ['0'..'9'] = lexNumber (c:cs) at
	|otherwise = error $ "Invalid character `" ++ [c] ++ "` in string"
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
specialWords = ["func", "while", "if", "else", "for", "var", "let", "return"]
