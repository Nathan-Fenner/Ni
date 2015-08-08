
{-# LANGUAGE NamedFieldPuns #-}

module Optimize where

import Compile
import Nice

simplify :: I -> Maybe I
simplify (ICall (ICall fun xs) ys) = Just $ ICall fun (xs ++ ys)
simplify (ICall fun []) = Just fun
simplify (IForce (IForce x)) = Just (IForce x)
simplify (IForce (IPartial fun n sofar))
	|length sofar == n = Just $ IForce $ IInvoke fun sofar
	|otherwise = Just $ IPartial fun n sofar
simplify (ICall (IPartial fun n sofar) args)
	|gap > 0 = Just $ ICall (IPartial fun n (sofar ++ take gap args)) (drop gap args)
	where
	gap = n - length sofar
simplify (IForce x@IOperator{}) = Just x
simplify (IForce x@IInt{}) = Just x
simplify (IForce x@IDecimal{}) = Just x
simplify (IForce x@IString{}) = Just x
simplify (IForce x@ILiteral{}) = Just x
simplify (ISequence [x]) = Just x
simplify _ = Nothing

simplifyTrees :: [I] -> Maybe [I]
simplifyTrees trees
	|any isJust trees' = Just $ zipWith (??) trees' trees
	|otherwise = Nothing
	where
	trees' = map simplifyTree trees
	isJust Just{} = True
	isJust _      = False

(??) :: Maybe a -> a -> a
Just x ?? _ = x
Nothing ?? y = y

simplifyTreeSafe :: I -> I
simplifyTreeSafe x = simplifyTree x ?? x

simplifyTree :: I -> Maybe I
simplifyTree x = case simplify x of
	Just x' -> Just $ simplifyTree x' ?? x'
	Nothing -> fmap simplifyTreeSafe $ simplifyTree' x

(?$) :: x -> (x -> Maybe x) -> (Maybe x, x)
(?$) x f = (f x, x)

simplifyTree' :: I -> Maybe I
simplifyTree' IName{} = Nothing
simplifyTree' IOperator{} = Nothing
simplifyTree' IInt{} = Nothing
simplifyTree' IDecimal{} = Nothing
simplifyTree' IString{} = Nothing
simplifyTree' ILiteral{} = Nothing
simplifyTree' (ICall fun args) = case check $ (fun ?$ simplifyTree) ?: (args ?$ simplifyTrees) ?: Nil of -- try the function
	Just (fun' ::: args' ::: Nil) -> Just $ ICall fun' args'
	Nothing                       -> Nothing
simplifyTree' (IInvoke fun args) = case simplifyTrees args  of
	Nothing    -> Nothing
	Just args' -> Just $ IInvoke fun args'
simplifyTree' (IForce value) = case simplifyTree value of
	Nothing     -> Nothing
	Just value' -> Just $ IForce value'
simplifyTree' (IPartial fun len args) = case simplifyTrees args of
	Just args' -> Just $ IPartial fun len args'
	Nothing    -> Nothing
simplifyTree' (IConstructor name fields) = case simplifyTrees $ map snd fields of
	Just fields' -> Just $ IConstructor name $ zip (map fst fields) fields'
	Nothing      -> Nothing
simplifyTree' (IDot left name) = case simplifyTree left of
	Just left' -> Just $ IDot left' name
	Nothing    -> Nothing
simplifyTree' (IAssign var value) = case check $ (var ?$ simplifyTree) ?: (value ?$ simplifyTree) ?: Nil  of
	Just (var' ::: value' ::: Nil) -> Just $ IAssign var' value'
	Nothing                        -> Nothing
simplifyTree' (IVar var) = case simplify var of
	Just var' -> Just $ IVar var'
	Nothing   -> Nothing
simplifyTree' (IVarAssign var value) = case check $ (var ?$ simplifyTree) ?: (value ?$ simplifyTree) ?: Nil of
	Just (var' ::: value' ::: Nil) -> Just $ IVarAssign var' value'
	Nothing                        -> Nothing
simplifyTree' (IIf con thenBody elseBody) = case check $ (con ?$ simplifyTree) ?: (thenBody ?$ simplifyTrees) ?: (elseBody ?$ simplifyTrees) ?: Nil of
	Just (con' ::: then' ::: else' ::: Nil) -> Just $ IIf con' then' else'
	Nothing                                 -> Nothing
	-- TODO: use actual sequencing for this?
simplifyTree' (IWhile con body) = case check $ (con ?$ simplifyTree) ?: (body ?$ simplifyTrees) ?: Nil of
	Just (con' ::: body' ::: Nil) -> Just $ IWhile con' body'
	Nothing                       -> Nothing
	-- TODO: use actual sequencing for this?
simplifyTree' (IDo thing) = case simplifyTree thing of
	Just thing' -> Just $ IDo thing'
	Nothing     -> Nothing
simplifyTree' (IFunc fun argNames body) = case simplifyTrees body of
	Just body' -> Just $ IFunc fun argNames body'
	Nothing    -> Nothing
simplifyTree' IBreak = Nothing
simplifyTree' (IReturn thing) = case simplifyTree thing of
	Just thing' -> Just $ IReturn thing'
	Nothing     -> Nothing
simplifyTree' (ISequence body) = case simplifyTrees body of
	Just body' -> Just $ ISequence body'
	Nothing    -> Nothing
simplifyTree' IComment{} = Nothing

optimizeProgram :: Compiled I -> Compiled I
optimizeProgram (Compiled funs body) = Compiled (map optimizeFunction funs) (simplifyTreeSafe body) where
	optimizeFunction (CompiledFunction funID funBody n) = CompiledFunction funID (simplifyTreeSafe funBody) n


{-
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

simplify :: I -> Optimized
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

inlineConstants c (IForce (IPartial fun len args))
	|length args < len = inlineConstants c $ IPartial fun len args
	|length args == len = inlineConstants c $ IInvoke fun args
inlineConstants c (ICall fun []) = inlineConstants c fun
inlineConstants c (ICall (IPartial fun len args) args')
	|count < len = inlineConstants c $ ICall (IPartial fun len (args ++ take extra args')) (drop extra args')
	where
	count = length args
	extra = len - count
inlineConstants c (IForce x@IInt{}) = inlineConstants c x

inlineConstants c (IInvoke fun args) = (c, IInvoke fun $ map (inliner c) args)
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

inlineConstants c (IForce (IPartial fun len args))
	|length args < len = inlineConstants c $ IPartial fun len args
	|length args == len = inlineConstants c $ IInvoke fun args
inlineConstants c (ICall fun []) = inlineConstants c fun
inlineConstants c (ICall (IPartial fun len args) args')
	|count < len = inlineConstants c $ ICall (IPartial fun len (args ++ take extra args')) (drop extra args')
	where
	count = length args
	extra = len - count
inlineConstants c (IForce x@IInt{}) = inlineConstants c x

inlineConstants c (IInvoke fun args) = (c, IInvoke fun $ map (inliner c) args)
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
inliner c x
	|x == next = x
	|otherwise = inliner c next
	where
	next = snd $ inlineConstants c x

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
isConstant IInvoke{} = False -- TODO: reconsider? (since this is like a force)
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
-}