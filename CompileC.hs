{-# LANGUAGE NamedFieldPuns #-}

module CompileC where

import System.IO.Unsafe(unsafePerformIO)

import Data.List(intercalate, sort)
import Control.Applicative
import Expression
import Lex(token)
import ParseType(niceType)

-- Don't inline it, so that we read the file at most once.
{-# NOINLINE preludeSource #-}
preludeSource :: String
preludeSource = unsafePerformIO $ readFile "prelude.c"

{-# NOINLINE preludeMain #-}
preludeMain :: String
preludeMain = unsafePerformIO $ readFile "preludeMain.c"

newtype FunctionID = FunctionID Int deriving (Show, Eq)

data IDGenerator = IDGenerator{next :: (FunctionID, IDGenerator)}

nextN :: Int -> IDGenerator -> ([FunctionID], IDGenerator)
nextN count givenGen = (\(x, y) -> (reverse x, y)) $ go count [] givenGen where
	go 0 result gen = (result, gen)
	go n result gen = go (n-1) (fid : result) gen' where
		(fid, gen') = next gen

functionIDGenerator :: Int -> IDGenerator
functionIDGenerator n = IDGenerator{next = (FunctionID n, functionIDGenerator (n + 1))}

newGenerator :: IDGenerator
newGenerator = functionIDGenerator 1

data CompiledFunction = CompiledFunction FunctionID I Int deriving Show

data Compiled a = Compiled [CompiledFunction] a deriving Show

data I
	= IName String -- a lexical name (in the sense)
	| IInt String
	| IDecimal String
	| IString String
	| ILiteral String
	| ICall I [I]
	| IForce I
	| IPartial FunctionID Int [I]
	| IConstructor String [(String, I)]
	| IDot I String
	| IAssign I I
	| IVar I
	| IVarAssign I I
	| IIf I [I] [I]
	| IWhile I [I]
	| IDo I
	| IFunc FunctionID [String] [I]
	| IBreak
	| IReturn I
	| ISequence [I]
	| IComment String
	deriving Show


tab :: String -> String
tab "" = ""
tab str = '\t' : tab' str where
	tab' "\n" = "\n"
	tab' "" = ""
	tab' ('\n':cs) = '\n' : '\t' : tab' cs
	tab' (c : cs) = c : tab' cs

serialize :: I -> String
serialize (IName ('$':n)) = n
serialize (IName n) = "_" ++ n
serialize (IInt i) = "MakeInt(" ++ i ++ ")"
serialize (IDecimal d) = "MakeDecimal(" ++ d ++ ")"
serialize (IString s) = "MakeString(" ++ s ++ ")"
serialize (ILiteral s) = s ++ "/*iliteral*/"
serialize (ICall f xs) = go (serialize f) xs
	where
	go fun [] = fun
	go fun (arg:args) = go ("Call(" ++ fun ++ ", " ++ serialize arg ++ ")") args
serialize (IForce x) = "Force(" ++ serialize x ++ ")"
serialize (IPartial funID capacity []) = "Partial( ( void(*)() )" ++ (serialize $ compileID funID) ++ ", " ++ show capacity ++ ")"
serialize (IPartial funID capacity args) = serialize $ (IPartial funID capacity []) `ICall` args
serialize (IConstructor name fields) = "Constructor(\"" ++ show name ++ "\", " ++ len ++ ", " ++ fieldNames ++ ", " ++ fieldValues ++ ")"
	where
	fieldNames = "(const char*[" ++ len ++ "]){" ++ intercalate ", " (map fieldNamer fields) ++ "}"
	fieldValues = "(Value[" ++ len ++ "]){" ++ intercalate ", " (map fieldValuer fields) ++ "}"
	fieldNamer (f, _) = "\"" ++ f ++ "\""
	fieldValuer (_, t) = serialize t
	len = show $ length fields
serialize (IDot left name) = "Dot(" ++ serialize left ++ ", \"" ++ show name ++ "\")"
serialize (IAssign v e) = serialize v ++ " = " ++ serialize e ++ ";\n"
serialize (IVar v) = "Value " ++ serialize v ++ ";\n"
serialize (IVarAssign v e) = "Value " ++ serialize v ++ " = " ++ serialize e ++ ";\n"
serialize (IIf c t []) = "if (Bool(" ++ serialize c ++ ")) {\n" ++ (tab $ concat $ map serialize t) ++ "}\n"
serialize (IIf c t e) = "if (Bool(" ++ serialize c ++ ")) {\n" ++ (tab $ concat $ map serialize t) ++ "} else {\n" ++ (tab $ concat $ map serialize e) ++ "}\n"
serialize (IWhile c b) = "while (Bool(" ++ serialize c ++ ")) {\n" ++ (tab $ concat $ map serialize b) ++ "}\n"
serialize (IDo e) = serialize e ++ ";\n"

serialize (IFunc funID args body) =
	"Value " ++ serialize (compileID funID) ++ "(" ++ intercalate ", " (map (("Value " ++) . serialize . IName) args) ++ ") {\n\tif(DEBUG)printf(\"@%d\\n\",__LINE__);\n" ++ (tab $ concat $ map serialize body) ++ "\treturn Unit;\n}\n"

serialize IBreak = "break;\n"
serialize (IReturn e) = "return VALIDATE(" ++ serialize e ++ ");\n"
serialize (ISequence s) = concat $ map serialize s
serialize (IComment comment) = "// " ++ comment ++ "\n"

instance Functor Compiled where
	fmap f (Compiled x a) = Compiled x (f a)

instance Applicative Compiled where
	pure x = Compiled [] x
	Compiled xs f <*> Compiled ys a = Compiled (xs ++ ys) (f a)

instance Monad Compiled where
	return = pure
	Compiled fs a >>= f = let Compiled gs b = f a in Compiled (gs ++ fs) b

newID :: Compiled a -> Compiled FunctionID
newID (Compiled fs _)  = Compiled fs (FunctionID (length fs))

addFunction :: FunctionID -> I -> Int -> Compiled ()
addFunction funID fun arity = Compiled [CompiledFunction funID fun arity] ()

compileID :: FunctionID -> I
compileID (FunctionID n) = IName ("Fun_" ++ show n)

----

mapGen :: IDGenerator -> [Expression] -> Compiled (IDGenerator, [I])
mapGen gen [] = return (gen, [])
mapGen gen (x:xs) = do
	(gen', x') <- compileExpression gen x
	(gen'', xs') <- mapGen gen' xs
	return (gen'', x' : xs')

mapGen' :: IDGenerator -> [Statement] -> Compiled (IDGenerator, [I])
mapGen' gen [] = return (gen, [])
mapGen' gen (x:xs) = do
	(gen', x') <- compileStatement gen x
	(gen'', xs') <- mapGen' gen' xs
	return (gen'', x' : xs')


isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

nub' :: [String] -> [String]
nub' list = go $ sort list where
	go [] = []
	go (x:x':xs)
		|x == x' = go (x:xs)
	go (x:xs) = x : go xs

less :: [String] -> [String] -> [String]
less list remove = filter (\l -> not $ l `elem` remove) list

----

containedVariables :: Expression -> [String]
containedVariables (ExpressionIdentifier name) = [token name]
containedVariables ExpressionIntegerLiteral{} = []
containedVariables ExpressionDecimalLiteral{} = []
containedVariables ExpressionStringLiteral{} = []
containedVariables ExpressionBoolLiteral{} = []
containedVariables ExpressionBang{} = []
containedVariables (ExpressionCall fun args) = nub' $ containedVariables fun ++ concat (map containedVariables args)
containedVariables ExpressionFunc{arguments, body} = containedVariables' body `less` map (token . fst) arguments
containedVariables (ExpressionOp left _ right) = nub' $ containedVariables left ++ containedVariables right
containedVariables (ExpressionPrefix _ arg) = containedVariables arg
containedVariables (ExpressionConstructor _ fields) = nub' $ concat $ map (containedVariables . snd) fields
containedVariables (ExpressionDot left _) = containedVariables left

containedVariables' :: [Statement] -> [String]
containedVariables' (StatementAssign _ value : ss) = nub' $ containedVariables value ++ containedVariables' ss
containedVariables' (StatementVarVoid name _ : ss) = containedVariables' ss `less` [token name]
containedVariables' (StatementVarAssign name _ value : ss) = nub' (containedVariables value ++ containedVariables' ss) `less` [token name]
containedVariables' (StatementDo e : ss) = nub' $ containedVariables e ++ containedVariables' ss
containedVariables' (StatementIf _ con body : ss) =
	nub' $ containedVariables con ++ containedVariables' body ++ containedVariables' ss
containedVariables' (StatementIfElse _ con bodyThen bodyElse : ss) =
	nub' $ containedVariables con ++ containedVariables' bodyThen ++ containedVariables' bodyElse ++ containedVariables' ss
containedVariables' (StatementWhile _ con body : ss) =
	nub' $ containedVariables con ++ containedVariables' body ++ containedVariables' ss
containedVariables' (StatementReturn _ (Just e) : ss) = nub' $ containedVariables e ++ containedVariables' ss
containedVariables' (StatementReturn _ Nothing : ss) = containedVariables' ss
containedVariables' (StatementBreak _ : ss) = containedVariables' ss
containedVariables' (StatementLet _ body : ss) =
	(nub' $ containedVariables' body ++ containedVariables' ss) `less` defined where
	defined = [ token name | StatementVarVoid name _ <- body ]
		++ [ token name | StatementVarAssign name _ _ <- body ]
		++ [ token funcName | StatementFunc{ funcName } <- body ]
containedVariables' (StatementFunc{ funcName, argumentsStatement, bodyStatement } : ss) =
	(nub' $ containedVariables' bodyStatement ++ containedVariables' ss) `less` ( token funcName : map (token . fst) argumentsStatement)
containedVariables' (StatementStruct{}:ss) = containedVariables' ss
containedVariables' [] = []

compileExpression :: IDGenerator -> Expression -> Compiled (IDGenerator, I)
compileExpression gen (ExpressionIdentifier t) = return (gen, IName $ token t)
compileExpression gen (ExpressionIntegerLiteral t) = return (gen, IInt $ token t)
compileExpression gen (ExpressionDecimalLiteral t) = return (gen, IDecimal $ token t)
compileExpression gen (ExpressionStringLiteral t) = return (gen, IString $ show $ token t)
compileExpression gen (ExpressionBoolLiteral t) =
	return (gen, ILiteral $ case token t of "True" -> "true"; "False" -> "false"; _ -> error "invalid boolean constant")
compileExpression gen (ExpressionBang _) = return (gen, IName "Bang")
compileExpression gen (ExpressionCall fun args) = do
	(gen', fun') <- compileExpression gen fun
	(gen'', args') <- mapGen gen' args
	return (gen'', ICall fun' args')
compileExpression gen e@ExpressionFunc{ arguments, funcBang, body } = do
	-- TODO: add implicit arguments
	let implicits = containedVariables e
	(gen', body') <- mapGen' gen body
	let (funID, gen'') = next gen'
	addFunction funID (IFunc funID (implicits ++ fullArguments) body') (length implicits + length fullArguments)
	return (gen'', IPartial funID (length fullArguments + length implicits) (map IName implicits))
	where
	fullArguments = map (token . fst) arguments ++ if isNothing funcBang then [] else ["Bang"]
compileExpression gen (ExpressionOp left op right) = do
	(gen', left') <- compileExpression gen left
	(gen'', right') <- compileExpression gen' right
	return (gen'', (IName $ "$Partial((void(*)())Operator" ++ concat (map speak $ token op) ++ ", 2)") `ICall` [left', right'])
	where
		speak '+' = "Plus"
		speak '-' = "Minus"
		speak '*' = "Star"
		speak '=' = "Equals"
		speak '?' = "Question"
		speak '>' = "More"
		speak '<' = "Less"
		speak '/' = "Divide"
		speak '^' = "Caret"
		speak '$' = "Dollar"
		speak '!' = "Bang"
		speak '&' = "And"
		speak '|' = "Or"
		speak '%' = "Percent"
		speak x = error $ "no known operator `" ++ show x ++ "`"
compileExpression gen (ExpressionPrefix op arg) = do
	(gen', arg') <- compileExpression gen arg
	return (gen', ICall (IName $ "$Prefix[" ++ show (token op) ++ "]") [arg'])
compileExpression gen (ExpressionConstructor name fields) = do
	(gen', fieldValues) <- mapGen gen (map snd fields)
	return (gen', IConstructor (niceType name) $ zip (map (token . fst) fields) fieldValues)
compileExpression gen (ExpressionDot left name) = do
	(gen', left') <- compileExpression gen left
	return (gen', IDot left' (token name))

compileStatement :: IDGenerator -> Statement -> Compiled (IDGenerator, I)
compileStatement gen (StatementAssign var value) = do
	(gen', value') <- compileExpression gen value
	return (gen', IAssign (IName $ token var) (IForce value'))
compileStatement gen (StatementVarVoid var _) = return (gen, IVar $ IName (token var))
compileStatement gen (StatementVarAssign var _ value) = do
	(gen', value') <- compileExpression gen value
	return (gen', IVarAssign (IName (token var)) (IForce value'))
compileStatement gen (StatementDo value) = do
	(gen', value') <- compileExpression gen value
	return (gen', IDo (IForce value'))
compileStatement gen (StatementIf _ con thenBlock) = do
	(gen', con') <- compileExpression gen con
	(gen'', thenBlock') <- mapGen' gen' thenBlock
	return (gen'', IIf (IForce con') thenBlock' [])
compileStatement gen (StatementIfElse _ con thenBlock elseBlock) = do
	(gen', con') <- compileExpression gen con
	(gen'', thenBlock') <- mapGen' gen' thenBlock
	(gen''', elseBlock') <- mapGen' gen'' elseBlock
	return (gen''', IIf (IForce con') thenBlock' elseBlock')
compileStatement gen (StatementWhile _ con block) = do
	(gen', con') <- compileExpression gen con
	(gen'', block') <- mapGen' gen' block
	return (gen'', IWhile (IForce con') block')
compileStatement gen (StatementReturn _ (Just value)) = do
	(gen', value') <- compileExpression gen value
	return (gen', IReturn (IForce value'))
compileStatement gen (StatementReturn _ Nothing) = return (gen, IReturn (IName "$Unit"))
compileStatement gen (StatementBreak _) = return (gen, IBreak)
compileStatement gen (StatementLet _ block) = do
	(gen', values) <- valuesOf gen $ filter (not . isStruct) block
	(gen'', contextualValues) <- makeContexts gen' values
	let letDeclares = map (\(n, v) -> IVarAssign (IName n) v) (zip letLetNames contextualValues)
	return (gen'', ISequence $ [IComment "begin let"] ++ (letDeclares ++ letAssembles) ++ [IComment "end"] )
	where
	isStruct StatementStruct{} = True
	isStruct _ = False
	letNames :: [String]
	letNames = map nameOf $ filter (not . isStruct) block where
		nameOf (StatementVarAssign var _ _) = token var
		nameOf StatementFunc{funcName} = token funcName
		nameOf s = error $ "not a valid let-member: " ++ show s
	letLetNames :: [String]
	letLetNames = map ("Let_" ++) letNames
	letAssembles = [ IVarAssign (IName n) (ICall (IName $ "Let_" ++ n) (map IName $ implicits ++ letLetNames)) | n <- letNames ]
	implicits :: [String]
	implicits = nub' $ concat (map (containedVariables' . (:[])) block) `less` letNames
	valuesOf mgen [] = return (mgen, [])
	valuesOf mgen (x : xs) = do
		(mgen', x') <- valueOf mgen x
		(mgen'', xs') <- valuesOf mgen' xs
		return (mgen'', x':xs')
	valueOf mgen (StatementVarAssign _ _ value) = compileExpression mgen value
	valueOf mgen StatementFunc{funcToken, genericsStatement, argumentsStatement, funcBangStatement, returnTypeStatement, bodyStatement} =
		compileExpression mgen (ExpressionFunc funcToken genericsStatement argumentsStatement funcBangStatement returnTypeStatement bodyStatement)
	valueOf _ _ = error "illegal let-member"
	makeContext :: IDGenerator -> I -> Compiled (IDGenerator, I)
	makeContext mgen value = do
		let (funID, mgen') = next mgen
		addFunction funID (IFunc funID (implicits ++ letLetNames) (preamble ++ [IReturn value]) ) (length implicits + length letLetNames)
		return (mgen', IPartial funID (length $ implicits ++ letNames) [])
		where
		preamble :: [I]
		preamble = [IVarAssign (IName n) (ICall (IName $ "Let_" ++ n) (map IName $ implicits ++ letLetNames)) | n <- letNames]
	makeContexts :: IDGenerator -> [I] -> Compiled (IDGenerator, [I])
	makeContexts mgen [] = return (mgen, [])
	makeContexts mgen (i:is) = do
		(mgen', i') <- makeContext mgen i
		(mgen'', is') <- makeContexts mgen' is
		return (mgen'', i' : is')
compileStatement gen StatementFunc{funcToken, funcName, argumentsStatement, funcBangStatement, returnTypeStatement, bodyStatement} =
	compileStatement gen $ StatementLet (error "let token is useless")
		[StatementVarAssign funcName (error "type is useless") $ ExpressionFunc funcToken (error "generics are useless") argumentsStatement funcBangStatement returnTypeStatement bodyStatement]
compileStatement gen (StatementStruct _ name generics _) = return (gen, IComment $ "struct " ++ token name ++ concat (map ((" " ++) . token) generics) )
	-- we will pass our implicits off as the shared "context" for evaluation.

headerGenerator :: CompiledFunction -> String
headerGenerator (CompiledFunction name _ n) = "Value " ++ (serialize $ compileID name) ++ "(" ++ intercalate ", " (take n $ repeat "Value") ++ ");\n"

compileProgram :: Statement -> String
compileProgram statement = case compileStatement newGenerator statement of
	Compiled headers (_, value) ->
		preludeSource ++
		"// forward feclarations\n" ++
		concat (map headerGenerator headers) ++
		"\n\n// function definitions\n" ++
		concat (map (\(CompiledFunction _ v _) -> serialize v) headers) ++
		"\n\n// Program\n\nint main() {\n" ++
		tab (preludeMain) ++ "\n" ++
		tab (serialize value) ++ "\n\n\t" ++
		"Force(Call(" ++ (serialize $ IName "main") ++ ", Bang));\n\treturn 0;\n}\n"
