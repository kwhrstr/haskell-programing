module Ch25_IdentityT where

newtype IdentityT f a  = IdentityT { runIdentityT :: f a}
  deriving (Eq, Show)
  
instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)
  
  
instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT $ pure x
  (IdentityT fab) <*> (IdentityT fa) = IdentityT $ fab <*> fa

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
  
