

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
	lr <- lift $ parseLR s
--	liftIO $ print lr
	l <- initL lr
--	liftIO $ print l
	k <- return $ getKind l
	v <- return $ Var k n
	liftIO $ do
		putStr $ showT v ++ " == "
		printT l
--		print l
	t <- getType l
--	t2 <- getType t
	liftIO $ do
		putStr $ showT v ++ " :: "
		printT t
		putStr $ "\n"
	addVarSpec v (Nothing, t)




main = run $ do
{-	define "pair" "#x:$$. _:$$"
	define "pcon" "#t:$$. @x:#t. @y:#t. _:(#pair #t)"
	define "int" "_:$$"
	define "one" "_:#int"
	define "x" "@pcon #int"
	define "y" "@pcon #int @one @one"-}

	define "and" "#a:$$. #b:$$. _:$$"
	define "proj1" "#a:$$. #b:$$. @x:(#and #a #b). _:#a"
	define "proj2" "#a:$$. #b:$$. @x:(#and #a #b). _:#b"
	define "comb" "#a:$$. #b:$$. @x:#a. @y:#b. _:(#and #a #b)"
	define "comutative" "#a:$$. #b:$$. @x:(#and #a #b). @comb #b #a (@proj2 #a #b @x) (@proj1 #a #b @x)"











