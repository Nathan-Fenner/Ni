
module Parse where

import Lex hiding(file)
import Control.Applicative

data Parse a = Parse { run :: (FilePath, [Token]) -> Result a }

data Result a = Success a [Token] | Error Bool String [Token] Location deriving Show

instance Functor Result where
	fmap f (Success x ts) = Success (f x) ts
	fmap _ (Error hard msg rest loc) = Error hard msg rest loc

instance Functor Parse where
	fmap f p = Parse $ \t -> fmap f $ run p t

instance Applicative Parse where
	pure a = Parse $ \(_, tokens) -> Success a tokens
	pf <*> pv = Parse $ \(file, tokens) -> case run pf (file, tokens) of
		Error hard e rest loc -> Error hard e rest loc -- an error has occurred
		Success f rest -> run (fmap f pv) (file, rest)

instance Monad Parse where
	return = pure
	x >>= f = Parse $ \(file, tokens) -> case run x (file, tokens) of
		Error hard e rest loc -> Error hard e rest loc -- an error has occurred
		Success v rest -> run (f v) (file, rest)

soften :: Parse a -> Parse a
soften f = Parse $ \tokens -> case run f tokens of
	Error True msg rest loc -> Error False msg rest loc
	x -> x

(|||) :: Parse a -> Parse a -> Parse a
f ||| g = Parse $ \tokens -> case run f tokens of
	Error False _ _ _ -> run g tokens
	Error True msg rest loc -> Error True msg rest loc
	x -> x

(&&&) :: Parse x -> Parse a -> Parse a
f &&& g = Parse $ \tokens -> case run f tokens of
	Error hard msg rest loc -> Error hard msg rest loc
	_ -> run g tokens

(???) :: Parse Bool -> (Parse a, Parse a) -> Parse a
c ??? (f, g) = do
	c' <- c
	if c' then f else g

accept :: Parse ()
accept = return ()

reject :: String -> Parse a
reject msg = Parse $ \(file, tokens) -> Error True msg tokens $ case tokens of
	[] -> FileEnd file
	(t:_) -> at t

parseMany :: Parse a -> Parse [a]
parseMany p = (do
	first <- p
	rest <- parseMany p
	return $ first : rest
	) ||| return []

parseMaybe :: Parse a -> Parse (Maybe a)
parseMaybe p = fmap Just (soften p) ||| return Nothing

peekMaybe :: Parse (Maybe Token)
peekMaybe = Parse $ \(_, tokens) -> case tokens of
	[] -> Success Nothing tokens
	(t : _) -> Success (Just t) tokens

parseManyWhile :: Parse Bool -> Parse a -> Parse [a]
parseManyWhile cond p = cond ??? (do
	first <- p
	rest <- parseManyWhile cond p
	return $ first : rest, return [])

parseManyUntil :: Parse Bool -> Parse a -> Parse [a]
parseManyUntil cond p = parseManyWhile (fmap not cond) p

expectToken :: (Token -> Bool) -> String -> Parse Token
expectToken predicate msg = Parse $ \(file, tokens) -> case tokens of
	[] -> Error True msg [] (FileEnd file)
	(t : ts) -> if predicate t then Success t ts else Error True msg (t:ts) (at t)

expectIdentifier :: String -> Parse Token
expectIdentifier msg = expectToken (\t -> kind t == Identifier) ("expected identifier: " ++ msg)

expectSpecial :: String -> String -> Parse Token
expectSpecial name msg = expectToken (\t -> kind t == Special && token t == name) msg

peekToken :: (Token -> Bool) -> Parse Bool
peekToken f = Parse $ \(_, ts) -> case ts of
	[] -> Success False ts
	(t:_) -> Success (f t) ts

peekTokenName :: String -> Parse Bool
peekTokenName name = peekToken (\t -> token t == name)

peekFileEnd :: Parse Bool
peekFileEnd = Parse $ \(_, ts) -> case ts of
	[] -> Success True ts
	_ -> Success False ts

checkToken :: (Token -> Bool) -> Parse Bool
checkToken f = Parse $ \(_, ts) -> case ts of
	[] -> Success False ts
	(t:rest) -> if f t then Success True rest else Success False (t:rest)

checkTokenName :: String -> Parse Bool
checkTokenName name = checkToken (\t -> token t == name)

maybeCheckTokenName :: String -> Parse (Maybe Token)
maybeCheckTokenName name = fmap Just (soften $ expectSpecial name "***") ||| return Nothing

lookOperators :: [String] -> Parse (Maybe Token)
lookOperators names = Parse $ \(_, ts) -> case ts of
	[] -> Success Nothing ts
	(t : rest) -> if kind t == Operator && token t `elem` names then Success (Just t) rest else Success Nothing ts
