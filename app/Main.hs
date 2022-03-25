{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveGeneric#-}
module Main where

import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger

import Control.Monad.IO.Class
import Data.String (fromString)
import System.Random
import Data.Aeson (FromJSON, ToJSON)
import Web.Scotty(scotty)
import Web.Scotty.Trans
import Data.Monoid (mconcat)
import GHC.Generics
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, lift, runReaderT)

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json; charset=utf-8\" if it has not already been set.
-- json :: ToJSON a => a -> ActionM ()
-- json = Trans.json

data AppMsg = AppMsg {code :: Int
                      , message :: String} deriving (Show, Eq, Generic)
instance ToJSON AppMsg
instance FromJSON AppMsg

newtype Config = Config
    { environment :: String} deriving (Eq, Read, Show)

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

-- application :: ScottyT Text ConfigM ()

-- http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html
main :: IO ()
-- See https://github.com/scotty-web/scotty/blob/480ed62a17dbadd5128b67f9a448339e52930c1f/examples/exceptions.hs
-- See Large args section of https://typeclasses.com/featured/dollar
main = scotty 3000 $ do
    get "/forbidden" $ do
        status status403
        json AppMsg {code = 403, message = "Forbidden"}
    get "/:word" $ do
        beam <- param "word"
        json AppMsg {code = 200, message = beam}
-- https://github.com/scotty-web/scotty/blob/c36f35b89993f329b8ff08852c1535816375a926/Web/Scotty.hs#L278
    notFound $ do
        json AppMsg {code = 404, message = "Not found"}

