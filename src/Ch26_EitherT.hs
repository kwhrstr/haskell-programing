module Ch26_EitherT where

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ (pure . pure) x
  EitherT emf <*> EitherT ema = EitherT $ (<*>) <$> emf <*> ema
  
instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ema) >>= f = EitherT $ do
    em <- ema
    case em of
      Left l -> return $ Left l
      Right r -> runEitherT $ f r

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema
  where
    swapEither (Right r) = Left r
    swapEither (Left l) = Right l

{-
  ema :: m (Either e a)
  Either e a -> Either a e
-}


eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT amb) = amb >>= either fa fb