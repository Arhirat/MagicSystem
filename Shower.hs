
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
	getShowTree (Var i n) = ShowNode $ (kindChars !! i) : n
	getShowTree Noname = ShowNode $ "_"
--	getShowTree (Unit i) = ShowNode $ (\c -> [c, c]) $ kindChars !! i
	getAFunc _ _ = ANone

instance ShowT L where
	getShowTree (Lam v t l) = ShowOp ". " 10 (ShowOp ":" 20 (getShowTree v) (getShowTree t)) (getShowTree l)
	getShowTree (Pi v t l) = ShowOp " -> " 10 (ShowOp ":" 20 (getShowTree v) (getShowTree t)) (getShowTree l)
	getShowTree (App l1 l2) = ShowOp " " 30 (getShowTree l1) (getShowTree l2)
--	getShowTree (Value v m) = ShowOp ":" 20 (ShowNode (showT v)) (getShowTree t)
	getShowTree (Value v m) = ShowNode (showT v)
	getShowTree (Unit i) = ShowNode $ (\c -> [c, c]) $ kindChars !! i
--	getShowTree (Sp (v, t)) = ShowOp ":" 20 (maybe (ShowNode "_") getShowTree v) (getShowTree t)
	getAFunc _ 10 = ARight
	getAFunc _ 20 = ARight
	getAFunc _ 30 = ALeft
	getAFunc _ _ = ANone

