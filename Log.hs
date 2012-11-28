{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}


module Log (
	Log(..),
	LiftLog(..),
	LogIO,
	withLogIO,
	showCallStack,
	withFunS,
	showLogS,
	up,
) where


import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Error



class (Monad m) => Log m where
	showLog :: String -> m ()
	withFun :: String -> m a -> m a
	getCallStack :: m [String]


instance Log IO where
	showLog s = putStr $ "log: " ++ s ++ "\n"
	withFun s a = do
		putStr $ "withFun <" ++ s ++ ">\n"
		r <- a
		putStr $ "exitFun <" ++ s ++ ">\n"
		return r
	getCallStack = return []



downT :: (MonadTrans t, Monad m) => (t m a -> b) -> m a -> b
downT f = \x -> f (lift x)

down :: (Monad m) => (m a -> b) -> a -> b
down f = \x -> f (return x)

up :: (Monad m) => (a -> b) -> (a -> m b)
up f = \x -> return $ f x

upT :: (MonadTrans t, Monad m) => (a -> m b) -> (a -> t m b)
upT f = \x -> lift $ f x


back :: (Monad m) => (a -> m b) -> m a -> m b
back fun ma = ma >>= fun

backT :: (Monad m, Monad (t m)) => (a -> t m b) -> t m a -> t m b
backT fun ma = ma >>= fun

ll :: (MonadTrans t, Monad m, Monad (t m)) =>(a -> m b) -> t m a -> t m b
ll fun = backT ( upT fun)

class (Monad m) => LiftLog m where
	liftLog :: LogIO a -> m a


instance LiftLog LogIO where
	liftLog = id

instance (LiftLog m) => LiftLog (StateT s m) where
	liftLog = lift . liftLog

instance (LiftLog m, Error s) => LiftLog (ErrorT s m) where
	liftLog = lift . liftLog


withFunS :: (LiftLog m) => String -> m a -> m a
withFunS s = back $ liftLog . (down $ withFun s)

showLogS :: (LiftLog m) => String -> m ()
showLogS = liftLog . showLog



newtype LogIO a = LogIO {runLogIO :: StateT [String] IO a} deriving (Monad, MonadIO)



instance Log LogIO where
	showLog s = liftIO $ putStr $ "log: " ++ s ++ "\n"
	withFun s a = do
		stack <- getCallStack
		liftIO $ evalStateT (runLogIO a) (s:stack)
	getCallStack = LogIO $ get


withLogIO :: LogIO a -> IO a
withLogIO (LogIO a) = evalStateT a []

showCallStack :: (Log m) => m ()
showCallStack = do
	cs <- getCallStack
	showLog $ "*" ++ (concat $ map ((++) " -> " ) $ reverse cs)


{-main :: IO ()
main = withLogIO $ do
	showLog "hello"
	withFun "a" $ do
		showCallStack
		showLog "de"
		withFun "b" $ do
			showCallStack
			showLog "rt"
	showLog "finish"-}

