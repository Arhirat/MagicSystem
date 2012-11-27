

module Shell where

import BaseType
import Reader
import Shower
import Core
import Log

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State

{-
runComp :: Context -> L -> IO ()
runComp context l = do
	print l
	when (not $ isUnit l) $ do
		rez <- return $ runIdentity $ runErrorT $ runStateT (getType l) context
		case rez of
			Left s -> putStr $ "> " ++ s
			Right (t, c) -> runComp context t
-}

run :: Compile LogIO () -> IO ()
run compile = do
	rez <- withLogIO $ runErrorT $ runStateT compile emptyContext
	case rez of
		Left s -> putStr $ "> " ++ s
		Right (t, c) -> putStr "> OK"


define :: String -> String -> Compile LogIO ()
define n s = do
	r <- lift $ parseL s
	k <- getKind r
	v <- return $ Var k n
	liftIO $ do
		putStr $ showT v ++ " := "
		printT r
		print r
	t <- getType r
	t2 <- getType t
	liftIO $ do
		putStr $ showT v ++ " :: "
		printT t
		putStr $ "\n"

	addVarType v t




main = run $ do
	define "and" "#q:$$. _:$$"
	define "Q" "_:$$"
	define "A" "_:#Q"
	define "B" "_:#Q"
	define "z" "#Q"
	define "X" "@and #Q @A @B "









