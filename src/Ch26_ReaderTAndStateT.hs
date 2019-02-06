{-# LANGUAGE InstanceSigs #-}
module Ch26_ReaderTAndStateT where
import Data.Bifunctor

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma
  
instance (Applicative m) => Applicative (ReaderT r m) where
  pure x = ReaderT $ (pure . pure) x
  (ReaderT rmf) <*> (ReaderT rma) =
    ReaderT $ (<*>) <$> rmf <*> rma
    
instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
newtype State s a = State { runState :: s -> (a, s) }
instance Functor (State s) where
  fmap f (State sa) = State $ \s ->
    let (a', s') = sa s
    in (f a', s')

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a  -> StateT s m b
  fmap f (StateT sma) = StateT $ \s ->
    let s' = sma s
    in first f <$> s'
{-
  Functor m => ( , ) a where
  (a, m b) -> m (a, b)
  fmap (a, ) mb

-}
  
instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smf) <*> (StateT sma) = StateT $ \s -> do
    (f, s') <- smf s
    (a, s'') <- sma s
    return (f a, s'')


instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

{-
  mf' :: m (a -> b, s)
  first :: (a -> b) -> p a c -> p b c
  first <$> mf' ::
 
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap <$> s' :: (a -> b)

  second :: (b -> c) -> p a b -> p a c
  
  


-}

--instance (Monad m) => Monad (StateT s m) where
--  return = pure
--  (ReaderT rma) >>= f = ReaderT $ \r -> do
--    a <- rma r
--    runReaderT (f a) r



