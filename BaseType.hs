

module BaseType (
	Var(..),
	L(..),
	LR(..),
	R(..),
	Context,
	contextGet,
	contextAdd,
	contextGetUnique,
	contextEmpty,
	contextPull,
	contextGetVarList,
	getKindChar,
	getKindIndex,
	scope,
	rGetType,
	rIsA,
	rAddDep,
	getKind,
	getKindVar,
	isUnit,
) where


--import Data.Map as MAP (Map, empty, member, insert, (!))
import Data.List (elemIndex, find)



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
	Value Var L |
	Undef L |
	Unit Int
 	deriving (Show)

data R = A L | S L deriving Show

data Context = Context {
	getVar :: [(Var, R)],
	getUnique :: Int
}

--type Spec = (Maybe L, L)

rGetType :: R -> L
rGetType (A t) = t
rGetType (S t) = t

rIsA :: R -> Bool
rIsA (A _) = True
rIsA _ = False

rAddDep :: Var -> L -> R -> R
rAddDep _ _ (A _) = undefined
rAddDep v t1 (S t2) = if getKind t1 == getKind t2
	then S (Pi t1 t2)
  	else S (Lam v t1 t2)


contextEmpty :: Context
contextEmpty = Context [] 0

contextGet :: Var -> (R -> a) -> (a) -> Context -> a
contextGet v f1 f2 c = case find (\(v1, _) -> v1 == v) (getVar c) of
	Just (_, r) -> f1 r
	Nothing -> f2

contextAdd :: Var -> R -> Context -> Context
contextAdd v r c = c {
	getVar = (v, r):(getVar c)
}

contextPull :: Var -> Context -> Maybe ([(Var, R)], Context)
contextPull v1 c = case split (\(v2, r) -> and [v1 == v2, rIsA r]) (getVar c) of
	Nothing -> Nothing
	Just (q, t) -> Just (q, c {getVar = t})

contextGetVarList :: Context -> [(Var, R)]
contextGetVarList = getVar


split :: (a -> Bool) -> [a] -> Maybe ([a], [a])
split  _ [] = Nothing
split f (h:t) = if f h
	then Just (h:[], t)
 	else case split f t of
		Nothing -> Nothing
		Just (q, t) -> Just (h:q, t)


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



--main = print $ split (\x -> x > 5) [3,6,8,3,5,11,6,4,7,34]