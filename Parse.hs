
module Parse where

import Lex
import Control.Applicative

data Parse a = Parse { run :: (FilePath, [Token]) -> Result a }

data Result a = Success a [Token] | Error String [Token] Location deriving Show

instance Functor Result where
	fmap f (Success x ts) = Success (f x) ts
	fmap _ (Error msg rest at) = Error msg rest at

instance Functor Parse where
	fmap f p = Parse $ \t -> fmap f $ run p t

instance Applicative Parse where
	pure a = Parse $ \(file, tokens) -> Success a tokens
	pf <*> pv = Parse $ \(file, tokens) -> case run pf (file, tokens) of
		Error e rest at -> Error e rest at -- an error has occurred
		Success f rest -> run (fmap f pv) (file, rest)

instance Monad Parse where
	return = pure
	x >>= f = Parse $ \(file, tokens) -> case run x (file, tokens) of
		Error e rest at -> Error e rest at -- an error has occurred
		Success v rest -> run (f v) (file, rest)

(|||) :: Parse a -> Parse a -> Parse a
f ||| g = Parse $ \tokens -> case run f tokens of
	Error _ _ _ -> run g tokens
	x -> x

(&&&) :: Parse x -> Parse a -> Parse a
f &&& g = Parse $ \tokens -> case run f tokens of
	Error msg rest at -> Error msg rest at
	_ -> run g tokens

(???) :: Parse Bool -> (Parse a, Parse a) -> Parse a
c ??? (f, g) = do
	c' <- c
	if c' then f else g

accept :: Parse ()
accept = return ()

reject :: String -> Parse a
reject msg = Parse $ \(file, tokens) -> Error msg tokens $ case tokens of
	[] -> FileEnd file
	(t:_) -> at t

parseMany :: Parse a -> Parse [a]
parseMany p = (do
	first <- p
	rest <- parseMany p
	return $ first : rest
	) ||| return []

parseMaybe :: Parse a -> Parse (Maybe a)
parseMaybe p = fmap Just p ||| return Nothing

peekMaybe :: Parse (Maybe Token)
peekMaybe = Parse $ \(file, tokens) -> case tokens of
	[] -> Success Nothing tokens
	(token : _) -> Success (Just token) tokens

parseManyWhile :: Parse Bool -> Parse a -> Parse [a]
parseManyWhile cond p = cond ??? (do
	first <- p
	rest <- parseManyWhile cond p
	return $ first : rest, return [])

parseManyUntil :: Parse Bool -> Parse a -> Parse [a]
parseManyUntil cond p = parseManyWhile (fmap not cond) p

expectToken :: (Token -> Bool) -> String -> Parse Token
expectToken predicate msg = Parse $ \(file, tokens) -> case tokens of
	[] -> Error msg [] (FileEnd file)
	(t : ts) -> if predicate t then Success t ts else Error msg (t:ts) (at t)

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

checkToken :: (Token -> Bool) -> Parse Bool
checkToken f = Parse $ \(_, ts) -> case ts of
	[] -> Success False ts
	(t:rest) -> if f t then Success True rest else Success False (t:rest)

checkTokenName :: String -> Parse Bool
checkTokenName name = checkToken (\t -> token t == name)

maybeCheckTokenName :: String -> Parse (Maybe Token)
maybeCheckTokenName name = fmap Just (expectSpecial name "***") ||| return Nothing

lookOperators :: [String] -> Parse (Maybe Token)
lookOperators names = Parse $ \(_, ts) -> case ts of
	[] -> Success Nothing ts
	(t : rest) -> if kind t == Operator && token t `elem` names then Success (Just t) rest else Success Nothing ts
