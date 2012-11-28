
module Core (
	getType,
	Compile,
	isUnit,
	getKind,
	addVarSpec,
	err,
	initL,
) where

import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Data.List (elemIndex)
import Debug.Trace


import BaseType
import Shower
import Log
import Change



type Compile m = StateT Context (ErrorT String m)


err :: (Monad m) => String -> Compile m a
err = lift . throwError



getVarSpec :: (Monad m) => Var -> Compile m Spec
getVarSpec v = get >>= contextGet v return (err $ "Variable " ++ showTS v ++ " not found")


addVarSpec :: (Monad m) => Var -> Spec -> Compile m ()
addVarSpec k t = modify (contextAdd k t)


getUniqueVar :: (Monad m) => Compile m String
getUniqueVar = do
	i <- StateT $ up contextGetUnique
	return $ "var" ++ show i



local :: (Monad m) => Compile m a -> Compile m a
local c = do
	s <- get
 	rez <- c
	put s
	return rez


getType :: (LiftLog m) => L -> Compile m L
getType (Unit i) = return$ Unit $ i+1
getType (Value v (_, t)) = return t
getType (Pi a b) = do
	k1 <- return $ getKind a
	k2 <- return $ getKind b
	when (k1 /= k2) $ err $ "Kind mistmatch1: " ++ showTS a ++ " " ++ showTS b
	return $ Unit $ k1 + 1

getType (Lam x a m) = do
	k1 <- return $ getKindVar x
	k2 <- return $ getKind a
	k3 <- return $ getKind m
	when (k1 + 1/= k2) $ err $ "Kind mistmatch2: " ++ showTS a ++ " " ++ showTS m
	b <- getType m
	case compare k1 k3 of
		LT -> err $ "Kind " ++ showTS x ++ " less whan " ++ showTS m
 		EQ -> return $ Pi a b
		GT -> return $ Lam x a b

getType (App a b) = do
	at <- getType a -- reduct
	apply at b

getType (Undef t) = return t




insert :: (Monad m) => L -> Var -> L -> Compile m L
insert l@(Value v1 m) v2 s = return $ if v1 == v2 then s else l
insert l@(Unit _) _ _ = return l
insert (App l1 l2) v s = do
	l1' <- insert l1 v s
 	l2' <- insert l2 v s
	return $ App l1' l2'
insert l@(Pi a b) v s = do
	a' <- insert a v s
	b' <- insert b v s
	return $ Pi a' b'
insert l@(Lam v1 a b) v2 s = do
	i <- getUniqueVar
	b <- renameVar b v1 i
	a <- insert a v2 s
	b <- insert b v2 s
	return $ Lam (ren i v1) a b

insert (Undef l) v s = do
	l' <- insert l v s
 	return $ Undef l'



renameVar :: (Monad m) => L -> Var -> String -> Compile m L
renameVar l@(Value v1 m) v2 s = return $ if v1 == v2
	then Value (ren s v1) m
 	else l
renameVar l@(Unit _) _ _ = return l
renameVar (App l1 l2) v s = do
	l1' <- renameVar l1 v s
 	l2' <- renameVar l2 v s
	return $ App l1' l2'
renameVar (Pi a b) v s = do
	a' <- renameVar a v s
	b' <- renameVar b v s
	return $ Pi a' b'
renameVar (Lam v1 a b) v2 s = do
	a' <- renameVar a v2 s
	if v1 == v2
		then return $ Lam v1 a' b
   		else do
			b' <- renameVar b v2 s
			return $ Lam v1 a' b'
renameVar (Undef l) v s = do
	l' <- renameVar l v s
	return $ Undef l'


ren :: String -> Var -> Var
ren s (Var i v) = Var i s




apply :: (LiftLog m, Monad m) => L -> L -> Compile m L
apply (Lam x t1 c) b = do
--	liftLog $ showLog "lambda app"
	t2 <- getType b
	eq <- runAllT $ eqL t1 t2
	when (not eq) $ err $ "Mismatch lam. Need " ++ showTS t1 ++ ", given " ++ showTS t2
	insert c x b

apply (Pi t1 c) b = do
	t2 <- getType b
--	liftLog $ showLog "pi app"
	eq <- runAllT $ eqL t1 t2
	when (not eq) $ err $ "Mismatch pi. Need " ++ showTS t1 ++ ", given " ++ showTS t2
	return c

apply t1 t2 = err $ "Can not apply" ++ showTS t1 ++ " to " ++ showTS t2


eqL :: (Monad m) => L -> L -> AllT (Compile m)
eqL (Value v1 _) (Value v2 _) = allT $ v1 == v2
eqL (Unit i1) (Unit i2) = allT $ i1 == i2
eqL (App a1 b1) (App a2 b2) = do
	eqL a1 a2
  	eqL b1 b2
eqL (Pi a1 b1) (Pi a2 b2) = do
	eqL a1 a2
	eqL b1 b2
eqL (Lam (Var i1 s1) a1 b1) (Lam v2@(Var i2 s2) a2 b2) = do
	when (i1 /= i2) $ allT False
	eqL a1 a2
	b2' <- lift $ renameVar b2 v2 s1
	eqL b1 b2'
eqL (Undef _) (Undef _) = allT False
eqL _ _ = allT False




initL :: (Monad m) => LR -> Compile m L
initL (AppR a b) = do
	a' <- initL a
 	b' <- initL b
	return $ App a' b'
initL (ValueR v) = do
	s <- getVarSpec v
	return $ Value v s
initL (UnitR i) = return $ Unit i
initL (PiR a b) = do
	a' <- initL a
 	b' <- initL b
	return $ Pi a' b'
initL (UndefR l) = do
	l' <- initL l
	return $ Undef l'
initL (LamR v a b) = do
	a' <- initL a
	b' <- local $ do
		addVarSpec v (Nothing, a')
		initL b
	return $ Lam v a' b'






