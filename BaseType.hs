

module BaseType (
	Var(..),
	L(..),
	LR(..),
	Spec,
	Context,
	contextGet,
	contextAdd,
	contextGetUnique,
	emptyContext,
	getKindChar,
	getKindIndex,
	scope,
	getKind,
	getKindVar,
	isUnit,
) where


import Data.Map as MAP (Map, empty, member, insert, (!))
import Data.List (elemIndex)



data Var = Var Int String deriving (Show, Ord, Eq)

data LR =
	LamR Var LR LR |
	PiR LR LR |
	AppR LR LR |
	ValueR Var |
	UndefR LR |
	UnitR Int
 	deriving (Show)


data L =
	Lam Var L L |
	Pi L L |
	App L L |
	Value Var Spec |
	Undef L |
	Unit Int
 	deriving (Show)

data Context = Context {
	getMap :: Map Var Spec,
	getUnique :: Int
}

type Spec = (Maybe L, L)



emptyContext :: Context
emptyContext = Context MAP.empty 0

contextGet :: Var -> (Spec -> a) -> (a) -> Context -> a
contextGet v f1 f2 c = if member v $ getMap c
	then f1 $ getMap c ! v
	else f2

contextAdd :: Var -> Spec -> Context -> Context
contextAdd k v c = c {
	getMap = insert k v $ getMap c
}

contextGetUnique :: Context -> (Int, Context)
contextGetUnique c = (getUnique c, c{getUnique = getUnique c + 1})




kindChars :: String
kindChars = "~!@#$%^"

getKindChar :: Int -> Char
getKindChar = (!!) kindChars

getKindIndex :: Char -> Maybe Int
getKindIndex = flip elemIndex kindChars


scope :: String -> String
scope s = "<" ++ s ++ ">"


isUnit :: L -> Bool
isUnit (Unit _) = True
isUnit _ = False


getKind :: L -> Int
getKind (Value v _) = getKindVar v
getKind (App a b) = getKind a
getKind (Lam v a b) = getKind b
getKind (Pi a b) = getKind b
getKind (Unit i) = i
getKind (Undef l) = getKind l - 1


getKindVar :: Var -> Int
getKindVar (Var i _) = i

