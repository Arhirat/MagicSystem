
module Reader (
	parseL,

) where

import Change
import BaseType
import Control.Monad.Error
import Data.List (elemIndex, null, isPrefixOf, length, elem)
--import Debug.Trace




half' :: String -> String -> Int -> ShowS -> (String, String)
half' a [] lev que = (que [], [])
half' a s@(h:t) lev que = if h == '('
	then half' a t (lev+1) $ (.) que $ (:) h
	else if h == ')'
		then half' a t (lev-1) $ (.) que $ (:) h
		else if and [isPrefixOf a s, lev == 0]
      		then (que [], drop (length a) s)
      		else half' a t lev $ (.) que $ (:) h


half a l = half' a l 0 id

halfBack a l = let (q, w) = half (reverse a) (reverse l) in (reverse w, reverse q)

type Parser m a = String -> ErrorT String m a


parseL :: (Monad m) => Parser m L
parseL = simplerParser $ choise [
	parseLam,
	parsePi,
--	parseSp,
	parseApp,
	parseUnit,
	parseValue
	]

choise :: (Monad m) => [Parser m a] -> Parser m a
choise [] s = throwError $ "Can not parse " ++ scope s
choise (h:t) s = catchError (h s) (\e -> choise t s)

parseHalf :: (Monad m) => Parser m a -> Parser m b -> String -> Bool -> (a -> b -> c) -> Parser m c
parseHalf pa pb c rev f s = do
	(a, b) <- if rev
		then return $ halfBack c s
		else return $ half c s
	when (b == []) $ throwError $ "Can not split " ++ scope s ++ " by '" ++ c ++ "'"
	a' <- pa a
	b' <- pb b
	return $ f a' b'

simple :: String -> String
simple = loopChange $ combChange [
	simple1,
	simple2,
	simple3
	]


emptyCharList = " \n"

simple1 :: String -> Change String
simple1 [] = return []
simple1 s@(h:t) = if elem h emptyCharList then returnChange t else return s

simple2 :: String -> Change String
simple2 [] = return []
simple2 s@(h:[]) = if elem h emptyCharList then returnChange [] else return s
simple2 (h:t) = do
	t' <- simple2 t
	return $ h:t'

simple3 :: String -> Change String
simple3 [] = return []
simple3 s@('(':t) = if null t
	then return s
	else if last t == ')' then returnChange (removeLast t) else return s
simple3 s = return s

removeLast (h:[]) = []
removeLast (h:t) = h : removeLast t

simplerParser :: Parser m a -> Parser m a
simplerParser p = p . simple


parseApp :: (Monad m) => Parser m L
parseApp = parseHalf parseL parseL " " True (\a b -> App a b)


parseLam :: (Monad m) => Parser m L
parseLam = parseHalf (parseHalf parseVar parseL ":" False (\a b -> (a, b))) parseL "." False (\(a, b) c -> Lam a b c)


parsePi :: (Monad m) => Parser m L
parsePi = parseHalf (parseHalf parseVar parseL ":" False (\a b -> (a, b))) parseL "->" False (\(a, b) c -> Pi a b c)


--parseSp :: (Monad m) => Parser m L
--parseSp = parseHalf (parseString "_") parseL ":" False (\a b -> Sp (Nothing, b))


parseString :: (Monad m) => String -> Parser m ()
parseString s s2 = if s == s2 then return () else throwError $ "Can not parse " ++ scope s


parseUnit :: (Monad m) => Parser m L
parseUnit [] = throwError $ "Empty unit "
parseUnit (h:[]) = throwError $ "Empty unit "
parseUnit (h:t:y) = do
	i <- parseKind (h:[])
	o <- parseKind (t:[])
	if i == o
		then return $ Unit i
		else throwError $ "Different unit"


parseValue :: (Monad m) => Parser m L
parseValue = parseHalf (simplerParser parseVar) (parseL2) ":" False (\a b -> Value a b)

parseL2 :: (Monad m) => Parser m (Maybe Spec)
parseL2 [] = return $ Nothing
parseL2 s = do
	l <- parseL s
	return $ Just (Nothing, l)


parseVar :: (Monad m) => Parser m Var
parseVar [] = throwError $ "Empty var name "
parseVar "_" = return $ Noname
parseVar s@(h:[]) = throwError $ "Short name " ++ scope s
parseVar s@(h:t) = do
	i <- parseKind (h:[])
	when (elem ' ' t) $ throwError $ "Incorrect name " ++ scope t
	when (elem '.' t) $ throwError $ "Incorrect name " ++ scope t
	when (elem '-' t) $ throwError $ "Incorrect name " ++ scope t
	when (elem '>' t) $ throwError $ "Incorrect name " ++ scope t
 	return $ Var i t



parseKind :: (Monad m) => Parser m Int
parseKind [] = throwError "Error1"
parseKind (c:[]) = do
	i <- return $ elemIndex c kindChars
	case i of
		Nothing -> throwError $ "No kind for char " ++ scope (c:[])
		Just x -> return x
parseKind _ = throwError $ "Error2"





{-
main = do
	r <- runErrorT $ parseL "!a:@@.!b:(@@) -> _:(!a !a !a)"
	print r
--	print $ simple " ( ( frf)  g  )  "
	print $ halfBack "->" "ded->gt->drfr"
-}




