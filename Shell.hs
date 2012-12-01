module Shell where



import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State

import BaseType
import Reader
import Shower
import Core
import Log


run :: Compile LogIO () -> IO ()
run compile = do
	rez <- withLogIO $ runErrorT $ runStateT compile contextEmpty
	case rez of
		Left s -> putStr $ "> " ++ show s
		Right (t, c) -> putStr "> OK"


set :: String -> String -> Compile LogIO ()
set n s = do
	v <- parse $ parseVar n
	lr <- parse $ parseLR s
	command $ CSet v lr
	check2 v

axiom :: String -> String -> Compile LogIO ()
axiom n s = do
	lr <- parse $ parseLR s
	v <- parse $ parseVar n
	command $ CAxiom v lr
	check2 v

close :: String -> [String] -> Compile LogIO ()
close a s = do
	av <- parse $ parseVar a
	sv <- parse $ mapM parseVar s
	command $ CClose av sv


check :: String -> Compile LogIO ()
check s = (parse $ parseVar s) >>= check2


check2 :: Var -> Compile LogIO ()
check2 v = do
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










