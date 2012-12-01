
module Reader (
	parseLR,
	parseVar,
) where

import Parser
import BaseType


parseLR :: (Monad m) => Parser m LR
parseLR = simplerParser $ choise [
	parseLamR,
	parsePiR,
	parseAppR,
	parseUnitR,
	parseValueR,
	parseUndefR
	]

parseAppR :: (Monad m) => Parser m LR
parseAppR = parseHalf parseLR parseLR " " True (\a b -> AppR a b)


parseLamR :: (Monad m) => Parser m LR
parseLamR = parseHalf (parseHalf parseVar parseLR ":" False (\a b -> (a, b))) parseLR "." False (\(a, b) c -> LamR a b c)


parsePiR :: (Monad m) => Parser m LR
parsePiR = parseHalf parseLR parseLR "->" False (\a b -> PiR a b)


parseUnitR :: (Monad m) => Parser m LR
parseUnitR [] = throwError $ "Empty unit "
parseUnitR (h:[]) = throwError $ "Empty unit "
parseUnitR (h:t:y) = do
	i <- parseKind (h:[])
	o <- parseKind (t:[])
	if i == o
		then return $ UnitR i
		else throwError $ "Different unit"

parseValueR :: (Monad m) => Parser m LR
parseValueR s = do
	v <- parseVar s
 	return $ ValueR v


parseUndefR :: (Monad m) => Parser m LR
parseUndefR ('_':':':t) = do
	l <- parseLR t
	return $ UndefR l
parseUndefR s = throwError $ "Can not parse undef " ++ show s


parseVar :: (Monad m) => Parser m Var
parseVar [] = throwError $ "Empty var name "
parseVar s@(h:[]) = throwError $ "Short name " ++ scope s
parseVar (h:t) = do
	i <- parseKind (h:[])
	when (elem ' ' t) $ throwError $ "Incorrect name " ++ scope t
	when (elem '.' t) $ throwError $ "Incorrect name " ++ scope t
	when (elem '-' t) $ throwError $ "Incorrect name " ++ scope t
	when (elem '>' t) $ throwError $ "Incorrect name " ++ scope t
 	return $ Var i t



parseKind :: (Monad m) => Parser m Int
parseKind [] = throwError "Error1"
parseKind (c:[]) = do
	i <- return $ getKindIndex c
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




