{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Ini where

import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import           Data.Char
import           Data.Map
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Text.IO as TIO
import           Text.RawString.QQ
import           Text.Trifecta
import           Test.Hspec


headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String deriving (Eq, Ord, Show)


