
{-# LANGUAGE TypeOperators, ImpredicativeTypes, MultiParamTypeClasses #-}

module Nice(Nil(Nil), (:|:)((:::)), (?:), check) where

data Nil = Nil deriving Show
data (:|:) h t = (:::) h t deriving Show
infixr 5 :::
infixr 5 :|:
data (:?:) h t = (:+:) h t | (:-:) h t deriving Show
infixr 5 :?:
infixr 5 :+:
infixr 5 :-:

(?:) :: (Maybe h, h) -> t -> (h :?: t)
(Nothing, x) ?: t = x :-: t
(Just x, _) ?: t = x :+: t

infixr 5 ?:


class HList list where
instance HList Nil
instance (HList t) => HList (h :|: t)

class HList list' => SList list list' where
	qux :: list -> (Bool, list')

instance SList Nil Nil where
	qux Nil = (False, Nil)
instance SList t t' => SList (h :?: t) (h :|: t') where
	qux (h :+: t) = (True, h ::: t')
		where (_, t') = qux t
	qux (h :-: t) = (hold, h ::: t')
		where (hold, t') = qux t

check :: SList t t' => t -> Maybe t'
check x = case qux x of
	(True, x') -> Just x'
	_ -> Nothing
