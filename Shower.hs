
module Shower (
	showT,
	showTS,
	printT,
) where


import ShowTree
import BaseType


showTS :: (ShowT a) => a -> String
showTS = scope . showT

instance ShowT Var where
	getShowTree (Var i n) = ShowNode $ getKindChar i : n
	getAFunc _ _ = ANone

instance ShowT L where
	getShowTree (Lam v t l) = ShowOp ". " 10 (ShowOp ":" 20 (getShowTree v) (getShowTree t)) (getShowTree l)
	getShowTree (Pi t l) = ShowOp " -> " 10 (getShowTree t) (getShowTree l)
	getShowTree (App l1 l2) = ShowOp " " 30 (getShowTree l1) (getShowTree l2)
	getShowTree (Value v m) = ShowNode (showT v)
	getShowTree (Unit i) = ShowNode $ (\c -> [c, c]) $ getKindChar i
	getShowTree (Undef l) = ShowOp ":" 20 (ShowNode "_") (getShowTree l)
	getAFunc _ 10 = ARight
	getAFunc _ 20 = ARight
	getAFunc _ 30 = ALeft
	getAFunc _ _ = ANone



instance Show Err where
	show (ErrSimple s) = s
	show (ErrVariableNotFound v) = "Variable " ++ showTS v ++ " not found"
	show (ErrKindMistmatch v l) = "Kind mistmatch " ++ showTS v ++ " and " ++ showTS l
	show (ErrKindMistmatch2 l1 l2) = "Kind mistmatch " ++ showTS l1 ++ " and " ++ showTS l2
	show (ErrApplyMistmatch l1 l2) = "Apply mistmatch " ++ showTS l1 ++ " and " ++ showTS l2
	show (ErrApply l1 l2) = "Can not apply " ++ showTS l1 ++ " to " ++ showTS l2
	show (ErrParse s) = "Parse error: " ++ s
