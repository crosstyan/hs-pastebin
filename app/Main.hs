{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveGeneric#-}
module Main where

import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Data.Text.Lazy (Text, pack)
import Control.Monad.IO.Class
import Data.String (fromString)
import System.Random
import Data.Aeson (FromJSON, ToJSON)
import Web.Scotty(scotty, ActionM)
import Web.Scotty.Trans
import Data.Monoid (mconcat)
import GHC.Generics
import Data.Default.Class (def)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, lift, runReaderT)
import Data.IORef (IORef)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IORef as IORef
data AppMsg = AppMsg {code :: Int
                      , message :: String} deriving (Show, Eq, Generic)
instance ToJSON AppMsg

data WordMsg = WordMsg {word :: String
                      , time :: Int} deriving (Show, Eq, Generic)
instance ToJSON WordMsg

data Config = Config
    { environment :: String
    -- https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-IORef.html
    , counts :: IORef (Map Text Integer)}

-- Provide a global environment for Scotty (Read Only)
newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

getTimes :: Map Text Integer -> Text -> (Map Text Integer ,Integer)
getTimes m k = case Map.lookup k m of
              -- Insert a new key and value in the map. If the key is already
              -- present in the map, the associated value is replaced with the
              -- supplied value. 
                Just n  -> (Map.insert k (n+1) m, n+1)
                Nothing -> (Map.insert k 0 m, 0)

application :: ScottyT Text ConfigM ()
application = do
  -- ActionT Text ConfigM 
  get "/" $ do
    e <- asks environment
    json AppMsg {code = 200, message = e}
  get "/forbidden" $ do
    status status403
    json AppMsg {code = 403, message = "Forbidden"}
  get "/:word" $ do
    c <- asks counts
    beam <- param "word"
    m <- liftIO (IORef.readIORef c :: IO (Map Text Integer))
    let (newM, times) = getTimes m $ fromString beam
    liftIO (IORef.writeIORef c newM)
    json WordMsg {word = beam, time = fromInteger times}
-- https://github.com/scotty-web/scotty/blob/c36f35b89993f329b8ff08852c1535816375a926/Web/Scotty.hs#L278
  notFound $ do
      json AppMsg {code = 404, message = "Not found"}

main :: IO ()
main = do
  c <- IORef.newIORef (Map.fromList [("", 0)] :: Map Text Integer)
  let config = Config { environment = "Development"
                      , counts = c}
-- let keyword equals (return just wrap it a monad and (<-) unwrap it)
-- config <- return $ Config { environment = "Development", counts = c}

-- Run monad m into IO, called at each action.
  let runIO m = runReaderT (runConfigM m) config

  scottyOptsT def runIO application

-- scottyOptsT
--   :: (Monad m, MonadIO n)	 
--   => Options	 -- See https://hackage.haskell.org/package/warp-3.3.11/docs/Network-Wai-Handler-Warp-Internal.html#t:Settings
--   -> (m Response -> IO Response)	
--   -> ScottyT e m ()	 -- not sure why Scotty needs e (Text) here. m is a Monad that runs in each action.
--   -> n ()

-- https://stackoverflow.com/questions/5545517/difference-between-state-st-ioref-and-mvar
-- http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html
-- See https://github.com/scotty-web/scotty/blob/480ed62a17dbadd5128b67f9a448339e52930c1f/examples/exceptions.hs
-- See Large args section of https://typeclasses.com/featured/dollar
-- https://github.com/scotty-web/scotty/blob/master/examples/reader.hs

