{-# LANGUAGE OverloadedStrings #-}
module Ch26_Scotty where

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (liftIO)
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.Except

import Control.Monad (liftM)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)

main =
  scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    liftIO $ putStrLn "hello"
    html $ mconcat ["<h1>Scotty,", beam, " me up!</h1>"]

liftReaderT :: m a -> ReaderT r m a
liftReaderT  = ReaderT . const

