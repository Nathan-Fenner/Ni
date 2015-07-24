
module ParseType where

import Lex
import Parse

data Type
	= TypeName Token
	| TypeCall Type [Type]
	| TypeBangArrow Type
	| TypeArrow Type Type
	deriving Show

niceType :: Type -> String
niceType (TypeName t) = token t
niceType (TypeCall fun args) = niceType fun ++ concat (map niceTypeArg args) where
	niceTypeArg (TypeName x) = token x
	niceTypeArg t = " (" ++ niceType t ++ ")"
niceType (TypeBangArrow right) = "! -> " ++ niceType right
niceType (TypeArrow left right) = niceTypeLeft left ++ " -> " ++ niceType right where
	niceTypeLeft (TypeName t) = token t
	niceTypeLeft t@TypeCall{} = niceType t
	niceTypeLeft t = "(" ++ niceTypeLeft t ++ ")"

(===) :: Type -> Type -> Bool
TypeName a === TypeName b = token a == token b
TypeCall f fArgs === TypeCall g gArgs = f === g && length fArgs == length gArgs && and (zipWith (===) fArgs gArgs)
TypeBangArrow a === TypeBangArrow b = a === b
TypeArrow a b === TypeArrow a' b' = a === a' && b === b'
_ === _ = False

makeType :: String -> Type
makeType name = TypeName (Token name (FileEnd "*") Identifier)

curryList :: Type -> ([Type], Type)
curryList (TypeArrow left arg) = let (args, r) = curryList arg in (left:args, r)
curryList t = ([], t)

parseTypeName :: Parse Type
parseTypeName = fmap TypeName (expectIdentifier "type name")

parseTypeParens :: Parse Type
parseTypeParens = do
	_ <- expectSpecial "(" "open paren"
	t <- parseType
	_ <- expectSpecial ")" "expected `)` to close `(`" -- TODO: add location of matching paren
	return t

parseTypeAtom :: Parse Type
parseTypeAtom = parseTypeName ||| parseTypeParens

parseTypeCall :: Parse Type
parseTypeCall = do
	fun <- parseTypeAtom
	args <- parseMany parseTypeAtom
	return $ case args of
		[] -> fun
		_ -> TypeCall fun args

parseTypeArrowBang :: Parse Type
parseTypeArrowBang = do
	_ <- expectSpecial "!" "type bang"
	_ <- expectSpecial "->" "arrow to follow bang type"
	right <- parseTypeArrow
	return $ TypeBangArrow right

parseTypeArrowOrdinary :: Parse Type
parseTypeArrowOrdinary = do
	left <- parseTypeAtom
	nextArrow <- checkToken (\t -> token t == "->")
	if nextArrow then do
		right <- parseTypeArrow
		return $ TypeArrow left right
	else
		return left
	
parseTypeArrow :: Parse Type
parseTypeArrow = peekToken (\t -> token t == "!") ??? (parseTypeArrowBang, parseTypeArrowOrdinary)

parseType :: Parse Type
parseType = parseTypeArrow