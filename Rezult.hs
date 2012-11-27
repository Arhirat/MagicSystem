module Rezult where

import Control.Monad.Trans


data Rezult b m r = Rezult {getRezult:: m (Either b r)}

instance (Monad m) => Monad (Rezult b m) where
	return x = Rezult $ return $ Right x
	(Rezult ma) >>= amb = Rezult $ do
 		a <- ma
 		case a of
			Left x -> return $ Left x
			Right a' -> getRezult $ amb a'
instance MonadTrans (Rezult b) where
	lift ma = Rezult $ ma >>= (return . Right)


breakT :: (Monad m) => a -> Rezult a m r
breakT x = Rezult $ return $ Left x
