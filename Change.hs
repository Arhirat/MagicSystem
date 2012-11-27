

module Change (
	Change,
 	markChange,
	loopChange,
	combChange,
	returnChange,
	AllT,
	allT,
	runAllT
	)
	where


import Control.Monad.Writer


type AllT m = WriterT All m ()

allT :: (Monad m) => Bool -> AllT m
allT = tell . All

runAllT :: (Monad m) => AllT m -> m Bool
runAllT a = do
	(_, y) <- runWriterT a
	return $ getAll y




data Change a = Change Bool a deriving Show


loopChange :: (a -> Change a) -> a -> a
loopChange f a = case f a of
	Change c a' -> if c then loopChange f a' else a


instance Monad Change where
	return x = Change False x
	(Change t1 a) >>= amb = let (Change t2 b) = amb a in Change (or [t1, t2]) b

markChange :: Change ()
markChange = Change True ()

returnChange :: a -> Change a
returnChange = Change True

combChange :: [a -> Change a] -> a -> Change a
combChange [] = \a -> return a
combChange (h:t) = \a -> do
	a' <- h a
	combChange t a'


