
{-# LANGUAGE NamedFieldPuns #-}

module Optimize where

import Compile
import Nice

simplify :: I -> Maybe I
simplify (ICall (ICall fun xs) ys) = Just $ ICall fun (xs ++ ys)
simplify (ICall fun []) = Just fun
simplify (IRemember x@IOperator{}) = Just x
simplify (IRemember x@IInt{}) = Just x
simplify (IRemember x@IDecimal{}) = Just x
simplify (IRemember x@IString{}) = Just x
simplify (IRemember x@ILiteral{}) = Just x
simplify (IRemember x@IPartial{}) = Just x
simplify (IRemember x@IConstructor{}) = Just x
simplify (IRemember (IRemember x)) = Just (IRemember x)
simplify (IForce (IRemember x)) = Just (IForce x)
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
simplify (IForce x@IConstructor{}) = Just x
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
simplifyTree' (IRemember value) = case simplifyTree value of
	Nothing     -> Nothing
	Just value' -> Just $ IRemember value'
simplifyTree' (IPartial fun len args) = case simplifyTrees args of
	Just args' -> Just $ IPartial fun len args'
	Nothing    -> Nothing
simplifyTree' (IConstructor name fields) = case simplifyTrees $ map snd fields of
	Just fields' -> Just $ IConstructor name $ zip (map fst fields) fields'
	Nothing      -> Nothing
simplifyTree' (IDot left name) = case simplifyTree left of
	Just left' -> Just $ IDot left' name
	Nothing    -> Nothing
simplifyTree' (IRef name ref) = case simplify ref of
	Just ref' -> Just $ IRef name ref'
	Nothing -> Nothing
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
