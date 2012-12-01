

module Shell where

import BaseType
import Reader
import Shower
import Core
import Log

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State


run :: Compile LogIO () -> IO ()
run compile = do
	rez <- withLogIO $ runErrorT $ runStateT compile contextEmpty
	case rez of
		Left s -> putStr $ "> " ++ s
		Right (t, c) -> putStr "> OK"


set :: String -> String -> Compile LogIO ()
set n s = do
	lr <- lift $ parseLR s
--	liftIO $ print lr
	l <- initL lr
--	liftIO $ print l
	k <- return $ getKind l
	v <- lift $ parseVar n
	t <- getType l
	addVarR v (S t)
--	liftIO $ putStr $ showT v ++ " == " ++ showT l ++ "\n"
	liftIO $ putStr $ showT v ++ " :: " ++ showT t ++ "\n"

axiom :: String -> String -> Compile LogIO ()
axiom n s = do
	lr <- lift $ parseLR s
	l <- initL lr
	v <- lift $ parseVar n
	addVarR v (A l)
	liftIO $ putStr $ showT v ++ " :: " ++ showT l ++ "\n"


close :: String -> [String] -> Compile LogIO ()
close a s = do
	av <- lift $ parseVar a
	sv <- lift $ mapM parseVar s
	closeAxiom av sv

check :: String -> Compile LogIO ()
check s = do
	v <- lift $ parseVar s
	t <- getVarType v
	liftIO $ putStr $ showT v ++ " :: " ++ showT t ++ "\n"


main = run $ do

	axiom "#and" "$$ -> $$ -> $$"
	axiom "@proj1" "#a:$$. #b:$$. #and #a #b -> #a"
	axiom "@proj2" "#a:$$. #b:$$. #and #a #b -> #b"
	axiom "@comb" "#a:$$. #b:$$. #a -> #b -> #and #a #b"
	axiom "#a" "$$"
	axiom "#b" "$$"
	axiom "@x" "#and #a #b"
	set "@x1" "@proj2 #a #b @x"
	set "@x2" "@proj1 #a #b @x"
	set "@comutative" "@comb #b #a @x1 @x2"
	close "#a" ["@comutative", "@x1"]
	check "@comutative"
	axiom "#x" "$$"
	axiom "#y" "$$"
	axiom "@q" "#and #x #y"
	set "@w" "@comutative #x #y @q"










