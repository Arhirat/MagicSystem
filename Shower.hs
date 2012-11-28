
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

