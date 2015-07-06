
module Parse where

import Lex
import Control.Applicative

data Parse a = Parse { run :: (FilePath, [Token]) -> Result a }

data Result a = Success a [Token] | Error String Location deriving Show

instance Functor Result where
	fmap f (Success x ts) = Success (f x) ts
	fmap _ (Error msg at) = Error msg at

instance Functor Parse where
	fmap f p = Parse $ \t -> fmap f $ run p t

instance Applicative Parse where
	pure a = Parse $ \(file, tokens) -> Success a tokens
	pf <*> pv = Parse $ \(file, tokens) -> case run pf (file, tokens) of
		Error e at -> Error e at -- an error has occurred
		Success f rest -> run (fmap f pv) (file, rest)

instance Monad Parse where
	return = pure
	x >>= f = Parse $ \(file, tokens) -> case run x (file, tokens) of
		Error e at -> Error e at -- an error has occurred
		Success v rest -> run (f v) (file, rest)

(|||) :: Parse a -> Parse a -> Parse a
f ||| g = Parse $ \tokens -> case run f tokens of
	Error _ _ -> run g tokens
	x -> x

accept :: Parse ()
accept = return ()

reject :: String -> Parse a
reject msg = Parse $ \(file, tokens) -> Error msg $ case tokens of
	[] -> FileEnd file
	(t:_) -> at t

expectToken :: (Token -> String) -> String -> Parse Token
expectToken predicate msg = Parse $ \(file, tokens) -> case tokens of
	[] -> Error msg (FileEnd file)
	(t : ts) -> if predicate t then Success t ts else Error msg (at t)

expectIdentifier :: String -> Parse Token
expectIdentifier msg = expectToken (\t -> kind t == Identifier) ("expected identifier: " ++ msg)

expectSpecial :: String -> String -> Parse Token
expectSpecial token msg = expectToken (\t -> kind t == Special && token t == token) ("expected identifier: " ++ msg)
