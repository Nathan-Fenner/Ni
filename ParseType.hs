
module ParseType where

import Lex
import Parse
import Data.List(intercalate)

data Type
	= TypeName Token
	| TypeCall Type [Type]
	| TypeBangArrow Token Type
	| TypeArrow Type Type
	| TypeGenerics [Token] Type
	deriving Show

data CanonicalType = CanonicalType String [Type]

-- canonicalType converts a type into its canonical form, which represents
-- concrete struct types.
canonicalType :: Type -> Either String CanonicalType
canonicalType (TypeName name) = Right $ CanonicalType (token name) []
canonicalType (TypeCall left args) = do
	CanonicalType name args' <- canonicalType left
	Right $ CanonicalType name (args' ++ args)
canonicalType t@TypeBangArrow{} = Left $ "a function type `" ++ niceType t ++ "` has no canonical type"
canonicalType t@TypeArrow{} = Left $ "a function type `" ++ niceType t ++ "` has no canonical type"
canonicalType t@TypeGenerics{} = Left $ "a generic declaration `" ++ niceType t ++ "` has no canonical type"

-- typeAt is a token which identifies the location in the source file of the
-- type.
typeAt :: Type -> Token
typeAt (TypeName name) = name
typeAt (TypeCall fun _) = typeAt fun
typeAt (TypeBangArrow bang _) = bang
typeAt (TypeArrow left _) = typeAt left
typeAt (TypeGenerics [] ty) = typeAt ty
typeAt (TypeGenerics (x:_) _) = x

-- niceType returns a pretty-printed description of the type.
niceType :: Type -> String
niceType (TypeName t) = token t
niceType (TypeCall fun args) = niceType fun ++ concat (map ((" "++) . niceTypeArg) args) where
	niceTypeArg (TypeName x) = token x
	niceTypeArg t = " (" ++ niceType t ++ ")"
niceType (TypeBangArrow _ right) = "! -> " ++ niceType right
niceType (TypeArrow left right) = niceTypeLeft left ++ " -> " ++ niceType right where
	niceTypeLeft (TypeName t) = token t
	niceTypeLeft t@TypeCall{} = niceType t
	niceTypeLeft t = "(" ++ niceTypeLeft t ++ ")"
niceType (TypeGenerics [] ty) = niceType ty
niceType (TypeGenerics gens ty) = "<" ++ intercalate ", " (map token gens) ++ "> " ++ niceType ty

-- left === right returns whether type descriptions left and right are
-- identical.
-- e.g., Int === t is FALSE.
(===) :: Type -> Type -> Bool
TypeName a === TypeName b = token a == token b
TypeCall f fArgs === TypeCall g gArgs = f === g && length fArgs == length gArgs && and (zipWith (===) fArgs gArgs)
TypeBangArrow _ a === TypeBangArrow _ b = a === b
TypeArrow a b === TypeArrow a' b' = a === a' && b === b'
_ === _ = False

makeType :: String -> Type
makeType name = TypeName (Token name (FileEnd "*") Identifier)

curryList :: Type -> ([Type], Type)
curryList (TypeArrow left arg) = let (args, r) = curryList arg in (left:args, r)
curryList t = ([], t)

parseTypeName :: Parse Type
parseTypeName = soften $ fmap TypeName (expectIdentifier "type name")

parseTypeParens :: Parse Type
parseTypeParens = do
	_ <- soften $ expectSpecial "(" "open paren"
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
	bangToken <- expectSpecial "!" "type bang"
	_ <- expectSpecial "->" "arrow to follow bang type"
	right <- parseTypeArrow
	return $ TypeBangArrow bangToken right

parseTypeArrowOrdinary :: Parse Type
parseTypeArrowOrdinary = do
	left <- parseTypeCall
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

replaceType :: (String, Type) -> Type -> Type
replaceType (name, with) (TypeName name')
	|name == token name' = with
	|otherwise = TypeName name'
replaceType pair (TypeCall fun args) = TypeCall (replaceType pair fun) (map (replaceType pair) args)
replaceType pair (TypeBangArrow bang right) = TypeBangArrow bang (replaceType pair right)
replaceType pair (TypeArrow left right) = replaceType pair left `TypeArrow` replaceType pair right
replaceType (name, with) (TypeGenerics generics body)
	| name `elem` map token generics = TypeGenerics generics body
	| otherwise = TypeGenerics generics $ replaceType (name, with) body

replaceTypes :: [(String, Type)] -> Type -> Type
replaceTypes [] t = t
replaceTypes (p:ps) t = replaceTypes ps (replaceType p t)

unifyWith :: [(String, Type)] -> [(String, Type)] -> Either String [(String, Type)]
unifyWith x y = do
	case [(nx, x', y') | (nx, x') <- x, (ny, y') <- y, nx == ny, not $ x' === y' ] of
		[] -> return ()
		fails -> Left $ intercalate ", " $ map (\(n, x', y') -> "type `" ++ n ++ "` cannot be unified with both `" ++ niceType x' ++ "` and `" ++ niceType y' ++ "`") fails
	return $ filter (`absent` y) x ++ y
	where
	(n, _) `found` list = n `elem` map fst list
	x' `absent` y' = not $ x' `found` y'

unifyAll :: [[(String, Type)]] -> Either String [(String, Type)]
unifyAll [] = Right []
unifyAll [x] = Right x
unifyAll (x:y:rest) = do
	z <- x `unifyWith` y
	unifyAll (z:rest)

-- unifyTypes checks whether the right is a type that could be assigned to the left.
-- for example, unifyTypes (Int) (<t> t) [] ==> Right[t = Int]
-- but unifyTypes (<t> t) (Int) [] ==> Left[can't unify `<t> t` with `Int`]
unifyTypes :: Type -> Type -> [String] -> Either String [(String, Type)]
unifyTypes t (TypeName name) free
	|token name `elem` free = Right [(token name, t)]
unifyTypes (TypeName left) (TypeName right) _
	|token left == token right = Right []
unifyTypes left (TypeCall fun []) free = unifyTypes left fun free
unifyTypes (TypeCall fun []) right free = unifyTypes fun right free
unifyTypes (TypeCall funLeft argsLeft) (TypeCall funRight argsRight) free = do
	final <- unifyTypes (last argsLeft) (last argsRight) free
	rest <- unifyTypes (TypeCall funLeft (init argsLeft)) (TypeCall funRight (init argsRight)) free
	final `unifyWith` rest
unifyTypes (TypeArrow leftLeft leftRight) (TypeArrow rightLeft rightRight) free = do
	bindLeft <- unifyTypes leftLeft rightLeft free
	bindRight <- unifyTypes leftRight rightRight free
	bindLeft `unifyWith` bindRight
unifyTypes (TypeBangArrow _ left) (TypeBangArrow _ right) free = unifyTypes left right free
unifyTypes left (TypeGenerics generics right) free = do
	result <- unifyTypes left right (map token generics ++ free)
	return $ filter (\(n, _) -> n `elem` free) result
unifyTypes left@TypeGenerics{} right _ = Left $ "cannot convert specific type `" ++ niceType right ++ "` into generic type `" ++ niceType left ++ "`"
unifyTypes left right _ = Left $ "cannot unify type `" ++ niceType left ++ "` with type `" ++ niceType right ++ "`"
