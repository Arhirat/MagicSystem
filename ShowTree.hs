
module ShowTree (
	A(..),
	ShowTree(..),
	ShowT(..),
	showT,
	printT,
) where


data A = ALeft | ARight | ANone

type AFunc = Int -> A

data ShowTree =
	ShowOp String Int ShowTree ShowTree |
  	ShowNode String

class ShowT a where
	getShowTree :: a -> ShowTree
	getAFunc :: a -> AFunc


showT :: (ShowT a) => a -> String
showT a = showTree (getAFunc a) (getShowTree a) []

printT :: (ShowT a) => a -> IO ()
printT a = putStr $ showT a ++ "\n"


showTree :: AFunc -> ShowTree -> ShowS
showTree fu (ShowNode s) = showString s
showTree fu (ShowOp s p t1 t2) = (cmpPrior fu t1 p False) . showString s . (cmpPrior fu t2 p True)


cmpPrior :: AFunc -> ShowTree -> Int -> Bool -> ShowS
cmpPrior fu t p pos = let
	sc = (case getPrior t of
		Nothing -> False
		Just p2 -> case compare p p2 of
 			LT -> False
			GT -> True
			EQ -> case fu p of
				ANone -> False
				ALeft -> pos
				ARight -> not pos)
	in if sc then scope $ showTree fu t else showTree fu t

scope :: ShowS -> ShowS
scope s = showChar '(' . s . showChar ')'


getPrior :: ShowTree -> Maybe Int
getPrior (ShowNode s) = Nothing
getPrior (ShowOp _ p _ _) = Just p


{-fun 10 = ARight
main = putStr $ showTree fun (ShowOp "*" 20
	(ShowOp "+" 10
		(ShowNode "a")
		(ShowNode "b"))
	(ShowOp "+" 10
		(ShowNode "c")
		(ShowNode "d")))[]
-}