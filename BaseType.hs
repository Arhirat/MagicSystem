

module BaseType (
	Var(..),
	L(..),
	Spec,
	Context,
	contextGet,
	contextAdd,
	emptyContext,
	kindChars,
	scope,
) where


import Data.Map as MAP (Map, empty, member, insert, (!))



data Var =
	Noname |
	Var Int String
	deriving (Show, Ord, Eq)

data L =
	Lam Var L L |
	Pi Var L L |
	App L L |
	Value Var (Maybe Spec) |
	Unit Int
 	deriving (Show)

data Context = Context {
	getMap :: Map Var Spec
}

type Spec = (Maybe L, L)



emptyContext :: Context
emptyContext = Context MAP.empty

contextGet :: Var -> (Spec -> a) -> (a) -> Context -> a
contextGet v f1 f2 c = if member v $ getMap c
	then f1 $ getMap c ! v
	else f2



contextAdd :: Var -> Spec -> Context -> Context
contextAdd k v c = c {
	getMap = insert k v $ getMap c
}


{-
instance Eq L where
	(App a b) == (App c d) = and [a == c, b == d]
	(Value v1) == (Value v2) = v1 == v2
	(Type v1) == (Type v2) = v1 == v2
	(Lam a b c) == (Lam q w e) = and [b == w, c == e]
	(Pi a b c) == (Pi q w e) = and [b == w, c == e]
	_ == _ = False
-}



kindChars :: String
kindChars = "~!@#$%^"


scope :: String -> String
scope s = "<" ++ s ++ ">"

