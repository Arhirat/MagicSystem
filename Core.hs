
module Core (
	getType,
	Compile,
	isUnit,
	getKind,
	addVarType,
	err,
) where

import BaseType
import Shower
import Log
import Change
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Data.List (elemIndex)

import Debug.Trace



type Compile m = StateT Context (ErrorT String m)


err :: (Monad m) => String -> Compile m a
err = lift . throwError



getVarType :: (Monad m) => Var -> Compile m L
getVarType v = get >>= contextGet v (\x -> return $ snd x) (err $ "Variable " ++ showTS v ++ " not found")


addVarType :: (Monad m) => Var -> L -> Compile m ()
addVarType k t = modify (contextAdd k (Nothing, t))



getType :: (LiftLog m) => L -> Compile m L
getType (Unit i) = return$ Unit $ i+1
getType (Value v (Just (_, t))) = return t
getType (Value v Nothing) = getVarType v
--getType (Sp (_, t)) = withFunS "getType1" $ return $ t
getType (Pi x a b) = do
	k1 <- return $ getKindVar x
	k2 <- getKind a
	when (k1 + 1 /= k2) $ err $ "Kind mistmatch1: " ++ showTS x ++ " " ++ showTS a
	t <- getType a
	when (not $ isUnit t) $ err $ showTS t ++ " is not unit1"
	b' <- insertSpec b x (Nothing, a)
	getType b'

getType (Lam x a m) = do
	k1 <- return $ getKindVar x
	k2 <- getKind a
	when (k1 + 1/= k2) $ err $ "Kind mistmatch2: " ++ showTS a ++ " " ++ showTS m
	m' <- insertSpec m x (Nothing, a)
	b <- getType m'
	z <- getType (Pi x a b)
	when (not $ isUnit z) $ err $ showTS z ++ " is not unit2"
 	return $ Pi x a b

getType (App a b) = do
	at <- getType a -- reduct
	bt <- getType b
	apply at bt b


apl2 :: (a -> b -> c -> d) -> (a -> b -> c) -> a -> b -> d
apl2 f x = \a b -> f a b $ x a b

insertSpec :: (Monad m) => L -> Var -> Spec -> Compile m L
insertSpec l@(Value v1 m) v2 s = return $ if v1 == v2 then Value v1 (Just s) else l
insertSpec l@(Unit _) _ _ = return l
insertSpec (App l1 l2) v s = do
	l1' <- insertSpec l1 v s
 	l2' <- insertSpec l2 v s
	return $ App l1' l2'
insertSpec l@(Pi v1 a b) v2 s = if v1 == v2
	then return l
   	else do
		a' <- insertSpec a v2 s
		b' <- insertSpec b v2 s
		return $ Pi v1 a' b'
insertSpec l@(Lam v1 a b) v2 s = if v1 == v2
	then return l
   	else do
		a' <- insertSpec a v2 s
		b' <- insertSpec b v2 s
		return $ Lam v1 a' b'
--insertSpec l@(Sp (m, t)) v s = return l

ren :: String -> Var -> Var
ren s (Var i v) = Var i s


renameVar :: (Monad m) => L -> Var -> String -> Compile m L
renameVar l@(Value v1 m) v2 s = return $ if v1 == v2
	then Value (ren s v1) m
 	else l
renameVar l@(Unit _) _ _ = return l
renameVar (App l1 l2) v s = do
	l1' <- renameVar l1 v s
 	l2' <- renameVar l2 v s
	return $ App l1' l2'
renameVar (Pi v1 a b) v2 s = do
	a' <- renameVar a v2 s
	if v1 == v2
		then return $ Pi v1 a' b
   		else do
			b' <- renameVar b v2 s
			return $ Pi v1 a' b'
renameVar (Lam v1 a b) v2 s = do
	a' <- renameVar a v2 s
	if v1 == v2
		then return $ Lam v1 a' b
   		else do
			b' <- renameVar b v2 s
			return $ Lam v1 a' b'
--renameVar l@(Sp (m, t)) v s = do
--	t' <- renameVar t v s
--	return $ Sp (m, t')






isUnit :: L -> Bool
isUnit (Unit _) = True
isUnit _ = False


getKind :: (Monad m) => L -> Compile m Int
getKind (Value v _) = return $ getKindVar v
getKind (App a b) = getKind a
getKind (Lam v a b) = do
	b' <- insertSpec b v (Nothing, a)
	getKind b'
--getKind (Sp (_, l)) = getKind l >>= \l -> return $ l - 1
getKind (Pi v a b) = do
	b' <- insertSpec b v (Nothing, a)
	getKind b'
getKind (Unit i) = return $ i


getKindVar :: Var -> Int
getKindVar (Var i _) = i



apply :: (Monad m) => L -> L -> L -> Compile m L
apply (Pi a t1 c) t2 v = do
	eq <- runAllT $ eqL t1 t2
	when (not eq) $ err $ "Mismatch. Need " ++ showTS t1 ++ ", given " ++ showTS t2
	c' <- insertSpec c a (Nothing, v)
	return c'
apply t1 t2 _ = err $ "Can not apply" ++ showTS t1 ++ " to " ++ showTS t2


eqL :: (Monad m) => L -> L -> AllT (Compile m)
eqL (Value v1 _) (Value v2 _) = allT $ v1 == v2
eqL (Unit i1) (Unit i2) = allT $ i1 == i2
eqL (App l1 l2) (App l3 l4) = do
	eqL l1 l3
  	eqL l2 l4
eqL (Pi (Var i1 s1) a1 b1) (Pi v2@(Var i2 s2) a2 b2) = do
	when (i1 /= i2) $ allT False
	eqL a1 a2
	b2' <- lift $ renameVar b2 v2 s1
	eqL b1 b2'
eqL (Lam (Var i1 s1) a1 b1) (Lam v2@(Var i2 s2) a2 b2) = do
	when (i1 /= i2) $ allT False
	eqL a1 a2
	b2' <- lift $ renameVar b2 v2 s1
	eqL b1 b2'
--eqL (Sp (_, t1)) (Sp (_, t2)) = do
--	eqL t1 t2
eqL _ _ = allT False




