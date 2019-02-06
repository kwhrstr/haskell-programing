{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
module Ch26_MonadTrans where


import Control.Monad.Trans.Class
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Data.Bifunctor (first)

newtype IdentityT m a = IdentityT { runIdentityT :: m a}
  deriving (Functor, Applicative, Monad)

instance MonadTrans IdentityT where
  lift = IdentityT
  
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}


instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma
  
instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT $ (pure . pure) x
  (MaybeT fab) <*> (MaybeT ma) =
    MaybeT $ (<*>) <$> fab <*> ma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT $ f y

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a}
  deriving (Functor)

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
instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a  -> StateT s m b
  fmap f (StateT sma) = StateT $ \s ->
    let s' = sma s
    in first f <$> s'

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


instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just
  {-
    MaybeT :: m (Maybe a) -> MaybeT m a
    fmap Just = m a -> m (Just a)
  -}


newtype EitherT e m a =EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right
  
instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)


instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO
  
instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = ReaderT . const . liftIO
  {-
    const . liftIO = IO a -> r -> m a
  -}
instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO











