{-# LANGUAGE NamedFieldPuns #-}

module Compile where

import System.IO.Unsafe(unsafePerformIO)

import Data.List(intercalate, sort)
import Control.Applicative
import Expression
import Lex

-- Don't inline it, so that we read the file at most once.
{-# NOINLINE preludeSource #-}
preludeSource :: String
preludeSource = unsafePerformIO $ readFile "prelude.js"

newtype FunctionID = FunctionID Int deriving (Show, Eq)

data IDGenerator = IDGenerator{next :: (FunctionID, IDGenerator)}

nextN :: Int -> IDGenerator -> ([FunctionID], IDGenerator)
nextN count givenGen = go count [] givenGen where
	go 0 result gen = (result, gen)
	go n result gen = go (n-1) (fid : result) gen' where
		(fid, gen') = next gen

functionIDGenerator :: Int -> IDGenerator
functionIDGenerator n = IDGenerator{next = (FunctionID n, functionIDGenerator (n + 1))}

newGenerator :: IDGenerator
newGenerator = functionIDGenerator 1000

data CompiledFunction = CompiledFunction FunctionID String deriving Show

data Compiled a = Compiled [CompiledFunction] a deriving Show

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

addFunction :: FunctionID -> String -> Compiled ()
addFunction fid body = Compiled [CompiledFunction fid body] ()

compileID :: FunctionID -> String
compileID (FunctionID n) = "_fun" ++ show n

----

mapGen :: IDGenerator -> [Expression] -> Compiled (IDGenerator, [String])
mapGen gen [] = return (gen, [])
mapGen gen (x:xs) = do
	(gen', x') <- compileExpression gen x
	(gen'', xs') <- mapGen gen' xs
	return (gen'', x' : xs')

mapGen' :: IDGenerator -> [Statement] -> Compiled (IDGenerator, [String])
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

containedVariables :: Expression -> [String]
containedVariables (ExpressionIdentifier name) = [token name]
containedVariables ExpressionIntegerLiteral{} = []
containedVariables ExpressionDecimalLiteral{} = []
containedVariables ExpressionStringLiteral{} = []
containedVariables ExpressionBang{} = []
containedVariables (ExpressionCall fun args) = nub' $ containedVariables fun ++ concat (map containedVariables args)
containedVariables ExpressionFunc{arguments, body} = containedVariables' body `less` map (token . fst) arguments
containedVariables (ExpressionOp left _ right) = nub' $ containedVariables left ++ containedVariables right
containedVariables (ExpressionPrefix _ arg) = containedVariables arg

containedVariables' :: [Statement] -> [String]
containedVariables' (StatementAssign _ value : ss) = nub' $ containedVariables value ++ containedVariables' ss
containedVariables' (StatementVarVoid name _ : ss) = containedVariables' ss `less` [token name]
containedVariables' (StatementVarAssign name _ value : ss) = (containedVariables value ++ containedVariables' ss) `less` [token name]
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
-- containedVariables' (s : _) = error $ show s
containedVariables' [] = []

compileStatement :: IDGenerator -> Statement -> Compiled (IDGenerator, String)
compileStatement gen (StatementAssign name value) = do
	(gen', value') <- compileExpression gen value
	return (gen', token name ++ " = " ++ value' ++ ";\n")
compileStatement gen (StatementVarVoid name _) = return (gen, "var " ++ token name ++ ";\n")
compileStatement gen (StatementVarAssign name _ value) = do
	(gen', value') <- compileExpression gen value
	return (gen', "var " ++ token name ++ " = " ++ value' ++ ";\n")
compileStatement gen (StatementDo value) = do
	(gen', value') <- compileExpression gen value
	return (gen', value' ++ ";\n")
compileStatement gen (StatementIf _ con body) = do
	(gen', con') <- compileExpression gen con
	(gen'', body') <- compileStatements gen' body
	return (gen'', "if (" ++ con' ++ ") {\n" ++ body' ++ "}\n")
compileStatement gen (StatementIfElse _ con bodyThen bodyElse) = do
	(gen', con') <- compileExpression gen con
	(gen'', bodyThen') <- compileStatements gen' bodyThen
	(gen''', bodyElse') <- compileStatements gen'' bodyElse
	return (gen''', "if (" ++ con' ++ ") {\n" ++ bodyThen' ++ "} else {\n" ++ bodyElse' ++ "}\n")
compileStatement gen (StatementWhile _ con body) = do
	(gen', con') <- compileExpression gen con
	(gen'', body') <- compileStatements gen' body
	return (gen'', "while (" ++ con' ++ ") {\n" ++ body' ++ "}\n")
compileStatement gen (StatementReturn _ (Just value)) = do
	(gen', value') <- compileExpression gen value
	return (gen', "return " ++ value' ++ ";\n")
compileStatement gen (StatementReturn _ Nothing) = do
	return (gen, "return;\n")
compileStatement gen (StatementBreak _) = do
	return (gen, "break;\n")
compileStatement gen (StatementLet _ body) = do
	-- Variables in a let-block may refer to one another.
	-- To allow this, they are all wrapped in functions taking no parameters, returning these values:
	--     var x = y + 1;
	--     var y = 2;
	--     var z = y + 3;
	-- becomes:
	--     var x = func { return y + 1; }
	--     var y = func { return 2; }
	--     var z = func { return y + 3; }
	-- we pull these functions out to the top: (in JS):
	--    function fun_70(x, y, z) { return y + 1; }
	--    function fun_71(x, y, z) { return 2; }
	--    function fun_72(x, y, z) { return y + 3; }
	-- in the code:
	--    var x = CALL(fun_70, [fun_70 fun_71 fun_72]);
	--    var y = CALL(fun_71, [fun_70 fun_71 fun_72]);
	--    var z = CALL(fun_72, [fun_70 fun_71 fun_72]);
	-- hence, we must know the names of what we are assigning,
	-- and whether they're already in scope, or need to be brought into scope.
	let names = map getName body
	let values = map getValue body
	let wrappedValues = map wrapValue values
	(gen', funRefs) <- mapGen gen wrappedValues
	-- funRefs will be expressions like
	--     CALL( FUNC_70, [x, y, z] )
	-- which means that all we have to do is figure out the FIDs of these,
	-- and use these!
	let fids = map (\s -> takeWhile (/= ',') $ drop 5 s) funRefs
	-- This will be over-zealous with the "var's", but no one should care for a while.
	-- TODO: remove the ones we don't use
	let noticeBefore = "// BEGIN LET\n"
	let before = concat $ map (\(name, fid) -> "var " ++ name ++ " = " ++ fid ++ ";\n" ) (zip names fids)
	let during = concat $ map (\(name, funRef) -> "var let_rec_" ++ name ++ " = " ++ funRef ++ ";\n") (zip names funRefs)
	let after = concat $ map (\name -> name ++ " = let_rec_" ++ name ++ ";\n") names
	let noticeAfter = "// END LET\n"

	return (gen', noticeBefore ++ before ++ during ++ after ++ noticeAfter)
	where
	getName :: Statement -> String
	getName (StatementAssign name _) = token name
	getName (StatementVarAssign name _ _) = token name
	getName (StatementFunc{ funcName }) = token funcName
	getName s = error $ "programming error: invalid statement type present in let-block: " ++ show s
	getValue :: Statement -> Expression
	getValue (StatementAssign _ value) = value
	getValue (StatementVarAssign _ _ value) = value
	getValue StatementFunc{ funcToken, argumentsStatement, funcBangStatement, returnTypeStatement, bodyStatement }
		= ExpressionFunc funcToken argumentsStatement funcBangStatement returnTypeStatement bodyStatement
	getValue s = error $ "programming error: invalid statement type present in let-block: " ++ show s
	wrapValue :: Expression -> Expression
	wrapValue value = ExpressionFunc (Token "func" (FileEnd "*") Special) [] Nothing Nothing [ StatementReturn (Token "return" (FileEnd "*") Special) (Just value) ]	
compileStatement gen StatementFunc{ funcToken, funcName, argumentsStatement, funcBangStatement, returnTypeStatement, bodyStatement }
	= compileStatement gen $
		StatementLet (Token "let" (FileEnd "*") Special) [
			StatementVarAssign funcName (error "TODO: ?") $
				ExpressionFunc funcToken argumentsStatement funcBangStatement returnTypeStatement bodyStatement
		]

compileStatements :: IDGenerator -> [Statement] -> Compiled (IDGenerator, String)
compileStatements gen [] = return (gen, "")
compileStatements gen (s:ss) = do
	(gen', s') <- compileStatement gen s
	(gen'', ss') <- compileStatements gen' ss
	return (gen'', s' ++ ss')

compileExpression :: IDGenerator -> Expression -> Compiled (IDGenerator, String)
compileExpression gen (ExpressionIdentifier name) = return (gen, token name)
compileExpression gen (ExpressionIntegerLiteral value) = return (gen, token value)
compileExpression gen (ExpressionDecimalLiteral value) = return (gen, token value)
compileExpression gen (ExpressionStringLiteral value) = return (gen, show $ token value)
compileExpression gen (ExpressionBang _) = return (gen, "BANG")
compileExpression gen (ExpressionCall fun args) = do
	(gen', fun') <- compileExpression gen fun
	(gen'', args') <- mapGen gen' args
	let sargs = "[" ++ intercalate ", " args' ++ "]"
	return (gen'', "CALL(" ++ fun' ++ ", " ++ sargs ++ ")")
compileExpression gen e@ExpressionFunc{arguments, funcBang, body} = do
	let (fid, gen') = next gen
	(gen'', sbody) <- compileStatements gen' body
	let extraArguments = containedVariables e
	let allArguments = extraArguments ++ map (token . fst) arguments
	let extractArguments = concat $ map (\(arg, i) -> "var " ++ arg ++ " = _ni_args[" ++ show i ++ "];\n") $ zip allArguments [0 :: Int ..]
	let cargs = "[" ++ intercalate ", " extraArguments ++ "]"
	let n = length allArguments + if isNothing funcBang then 0 else 1
	addFunction fid $ "{nargs: " ++ show n ++ ", fun:\nfunction(_ni_args) {\n" ++ extractArguments ++ sbody ++ "}\n}"
	return (gen'', "CALL(" ++ compileID fid ++ ", " ++ cargs ++ ")")
compileExpression gen (ExpressionOp left op right) = do
	(gen', left') <- compileExpression gen left
	(gen'', right') <- compileExpression gen' right
	let funName = case token op of
		"+" -> "ADD"
		"-" -> "SUBTRACT"
		"*" -> "MULTIPLY"
		"/" -> "DIVIDE"
		"%" -> "MODULO"
		"++" -> "CONCAT"
		"==" -> "EQUAL"
		"/=" -> "NOT_EQUAL"
		">=" -> "GREATER_OR_EQUAL"
		"<=" -> "LESS_OR_EQUAL"
		">" -> "GREATER"
		"<" -> "LESS"
		o -> error $ "programming error: infix operator `" ++ o ++ "` does not exist"
	return $ (gen'', funName ++ "(" ++ left' ++ ", " ++ right' ++ ")")
compileExpression gen (ExpressionPrefix op arg) = do
	(gen', arg') <- compileExpression gen arg
	let funName = case token op of
		"-" -> "NEGATE"
		o -> error $ "programming error: prefix operator `" ++ o ++ "` does not exist"
	return $ (gen', funName ++ "(" ++ arg' ++ ")")

compileProgram :: Statement -> String
compileProgram statement = preludeSource ++ concat (map renderPrelude prelude) ++ "\n\n" ++ line
	where
	Compiled prelude (_, line) = compileStatement newGenerator statement
	renderPrelude (CompiledFunction fid object) = "var " ++ compileID fid ++ " = " ++ object ++ ";\n"
