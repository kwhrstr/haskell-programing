{-# LANGUAGE InstanceSigs #-}
module Ch25_Compose where

newtype Compose f g a = Compose { getCompose :: f (g a)}
  deriving (Eq, Show)
  
instance (Functor f, Functor g) =>
  Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga
  
instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a  = Compose $ (pure . pure) a
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose f <*> Compose a = Compose $  ((<*>) <$> f)  <*>  a
{-
 (<*>) :: (g (a -> b)) -> g a -> g b
 (<$>) :: ((g (a -> b)) -> g a -> g b) -> f g (a->b) -> f(g a ->  g b)
-}


instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m =>  (a -> m) -> Compose f g a -> m
  foldMap m' (Compose fga) = (foldMap . foldMap) m' fga
{-
  foldMap :: Monoid m => (a -> m) -> t a -> m
  fga :: f (g a)
  foldMap :: Monoid m => (g a -> m) -> f (g a) -> m
  foldMap :: Monoid m => (a -> m) -> g a -> m
-}

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative f' => (a -> f' b) -> Compose f g a -> f' (Compose f g b)
  traverse t' (Compose fga) =  Compose <$> (traverse . traverse) t' fga
  
{-
  fga :: f (g a)
  traverse :: Applicative f' => (a -> f' b) -> Compose f g a -> f' (Compose f g b)
  traverse :: Applicative m => (g a -> m (g b)) -> f (g a)  -> m (f g b)
  traverse :: Applicative m => (a -> m b) -> g a -> m (g b)
  traverse . traverse ::  (a -> m b) -> f (g a) -> m (f g b)
-}

class BiFunctor p where
  {-# MINIMAL biMap | first, second #-}
  biMap :: (a -> b) -> (c -> d) -> p a c -> p b d
  biMap f g = first f . second g
 
  first :: (a -> b) -> p a c -> p b c
  first f = biMap f id
  
  second :: (b -> c) -> p a b -> p a c
  second = biMap id
 
data Deux a b = Deux a b
instance BiFunctor Deux where
  biMap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
  biMap f g (Deux a c) = Deux (f a) (g c)

newtype Const a b = Const a
instance BiFunctor Const where
  biMap :: (a -> b) -> (c -> d) -> Const a c -> Const b d
  biMap f _ (Const a) = Const $ f a

data Drei a b c = Drei a b c
instance BiFunctor (Drei a) where
  biMap f g (Drei a b c) = Drei a (f b) (g c)


data SuperDrei a b c = SuperDrei a b
instance BiFunctor  (SuperDrei a) where
  biMap f _ (SuperDrei a b) = SuperDrei a (f b)

newtype SemiDrei a b c = SemiDrei a
instance BiFunctor (SemiDrei a) where
  biMap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d
instance BiFunctor (Quadriceps a b) where
  biMap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)
  
instance BiFunctor Either where
  biMap f _ (Left a) = Left $ f a
  biMap _ g (Right b) = Right $ g b














  


