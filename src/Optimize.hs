
{-# LANGUAGE NamedFieldPuns #-}

module Optimize where

import Compile

optimizeProgram :: Compiled I -> Compiled I
optimizeProgram (Compiled funs x) = Compiled (map fixup funs) (inliner base x)
	where
	base = Constant {
		known =
			[ ("print", IPartial (FunctionRaw "print_foreign") 2 [])
			],
		suppress = []
	}
	fixup (CompiledFunction name value len) = CompiledFunction name (inliner base value) len

data Constant = Constant
	{ known :: [(String, I)]
	, suppress :: [String]
	} deriving Show
addSuppress :: Constant -> String -> Constant
addSuppress c name
	|name `elem` suppress c = c
	|otherwise = c{suppress = name : suppress c}

addConstant :: Constant -> String -> I -> Constant
addConstant c name value = c{known = (name, value) : known c}

inlineConstants :: Constant -> I -> (Constant, I)
inlineConstants c (IName name)
	|name `elem` suppress c = (c, IName name)
	|otherwise = case lookup name (known c) of
		Nothing -> (c, IName name)
		Just value -> (c, value)
inlineConstants c (IOperator name) = (c, IOperator name) -- TODO
inlineConstants c x@IInt{} = (c, x)
inlineConstants c x@IDecimal{} = (c, x)
inlineConstants c x@IString{} = (c, x)
inlineConstants c x@ILiteral{} = (c, x)

inlineConstants c (ICall fun []) = inlineConstants c fun
inlineConstants c (ICall (IPartial fun len args) args')
	|count < len = inlineConstants c $ ICall (IPartial fun len (args ++ take extra args')) (drop extra args')
	where
	count = length args
	extra = len - count
inlineConstants c (IForce x@IInt{}) = inlineConstants c x

inlineConstants c (ICall fun args) = (c, ICall (inliner c fun) (map (inliner c) args))
inlineConstants c (IForce value) = (c, IForce $ inliner c value)
inlineConstants c (IPartial fun len args) = (c, IPartial fun len $ map (inliner c) args)
inlineConstants c (IConstructor name fields) = (c, IConstructor name $ map (\(k, v) -> (k, inliner c v)) fields)
inlineConstants c (IDot left name) = (c, IDot (inliner c left) name)
inlineConstants c (IAssign (IName name) right) = (addSuppress c name, IAssign (IName name) (inliner c right))
inlineConstants _ (IAssign _ _) = error "violates rules"
inlineConstants c (IVar name) = (c, IVar name)
inlineConstants c (IVarAssign (IName name) value)
	|isConstant value' = (addConstant c name value', IVarAssign (IName name) value')
	|otherwise = (c, IVarAssign (IName name) value')
	where
	value' = inliner c value
inlineConstants c (IVarAssign name value) = (c, IVarAssign name (inliner c value))
inlineConstants c (IIf con bodyThen bodyElse) = (c', IIf (inliner c con) bodyThen' bodyElse')
	where
	(ct, bodyThen') = inlineConstants' c bodyThen
	(ce, bodyElse') = inlineConstants' c bodyElse
	c' = c{suppress = suppress c ++ suppress ct ++ suppress ce}
inlineConstants c (IWhile con body) = (c', IWhile (inliner c con) body')
	where
	(bc, body') = inlineConstants' c body
	c' = c{suppress = suppress c ++ suppress bc}
inlineConstants c (IDo value) = (c, IDo $ inliner c value)
inlineConstants c (IFunc fun args body) = (c, IFunc fun args $ snd $ inlineConstants' c body)
inlineConstants c IBreak = (c, IBreak)
inlineConstants c (IReturn value) = (c, IReturn $ inliner c value)
inlineConstants c (ISequence body) = (\(x,y) -> (x, ISequence y)) $ inlineConstants' c body
inlineConstants c x@IComment{} = (c, x)

inliner :: Constant -> I -> I
inliner c x = snd $ inlineConstants c x

inlineConstants' :: Constant -> [I] -> (Constant, [I])
inlineConstants' c [] = (c, [])
inlineConstants' c (x:xs) = (c'', x' : xs')
	where
	(c', x') = inlineConstants c x
	(c'', xs') = inlineConstants' c' xs

isConstant :: I -> Bool
isConstant IName{} = False
isConstant IOperator{} = True
isConstant IInt{} = True
isConstant IDecimal{} = True
isConstant IString{} = True
isConstant ILiteral{} = True
isConstant (ICall fun args) = isConstant fun && all isConstant args
isConstant (IForce _) = False -- TODO: reconsider?
isConstant (IPartial _ _ args) = all isConstant args
isConstant (IConstructor _ fields) = all (isConstant . snd) fields
isConstant IDot{} = False -- TODO: reconsider?
-- These aren't actual expressions
isConstant IAssign{} = False
isConstant IVar{} = False
isConstant IVarAssign{} = False
isConstant IIf{} = False
isConstant IWhile{} = False
isConstant IDo{} = False
isConstant IFunc{} = False
isConstant IBreak{} = False
isConstant IReturn{} = False
isConstant ISequence{} = False
isConstant IComment{} = False
