{-# LANGUAGE InstanceSigs #-}
module Ch26_MaybeT where

import Data.Functor.Identity (Identity)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma
  
instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT $ (pure . pure) x
  (MaybeT fab) <*> (MaybeT ma) =
    MaybeT $ (<*>) <$> fab <*> ma
    
innerMost :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

second' :: [Maybe (Identity a -> Identity b)] -> [Maybe (Identity a) -> Maybe (Identity b)]
second' = fmap (<*>)

final' :: [Maybe (Identity a) -> Maybe (Identity b)] -> [Maybe (Identity a)] -> [Maybe (Identity b)]
final' = (<*>)

lmiApply :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a)] -> [Maybe (Identity b)]
lmiApply  = final' . second' . innerMost


instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT $ f y

