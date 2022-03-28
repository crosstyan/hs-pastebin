{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveGeneric#-}
module Main where

import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Data.Text.Lazy (Text, pack, unpack)
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
import qualified Database.Redis as Redis
import qualified Data.IORef as IORef
import qualified Data.UUID as UUID
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import Data.ByteString.UTF8 as BSU      -- from utf8-string
import qualified System.Random as UUID
import Data.Time.Clock
import Data.Time.Calendar

data ExpireConfig = Expire {
      min_age :: Integer
    , max_age :: Integer
    , max_size :: Integer}
data AppMsg = AppMsg {code :: Int
                      , message :: String} deriving (Show, Eq, Generic)
instance ToJSON AppMsg


data FileMsg = FileMsg { expire_time :: String
                        , file_size :: Integer
                        , uuid :: String} deriving (Show, Eq, Generic)
instance ToJSON FileMsg

data WordMsg = WordMsg {word :: String
                      , times :: Int} deriving (Show, Eq, Generic)
instance ToJSON WordMsg

data Config = Config
    { environment :: String
    -- https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-IORef.html
    , counts :: IORef (Map Text Integer)
    , conn :: Redis.Connection
    , expire :: ExpireConfig}

-- Provide a global environment for Scotty (Read Only)

-- runReaderT :: ReaderT r m a -> r -> m a
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


handleEx :: Monad m => Text -> ActionT Text m ()
handleEx e = do
    status status500
    json AppMsg {code = 500, message = unpack e}


handle404 :: Monad m => Text -> ActionT Text m ()
handle404 e = do
    status status404
    json AppMsg {code = 404, message = unpack e}


handle400 :: Monad m => Text -> ActionT Text m ()
handle400 e = do
    status status400
    json AppMsg {code = 400, message = unpack e}


-- retention = min_age + (-max_age + min_age) * pow((file_size / max_size - 1), 3)
-- From http://0x0.st/

retention :: (Fractional a) => a -> a -> a -> a -> a
retention min_age max_age max_size file_size = min_age + (-max_age + min_age) * (file_size / max_size - 1) ^ 3


-- Get a parameter. First looks in captures, then form data, then query parameters.
-- * Raises an exception which can be caught by 'rescue' if parameter is not found.
-- param

-- lift   :: (Monad m, MonadTrans t) => m a -> t m a
-- return ::                              a -> m a

-- ScottyT :: * -> (* -> *) -> * -> *
-- ScottyT    e       m        a      -- what is a?
-- ScottyT Text IO ::          * -> * (ScottyM)
-- ActionT :: * -> (* -> *) -> * -> *
-- ActionT    e       m        a
-- ActionT Text ConfigM ::     * -> *
-- a is some generic type? Unit?
--                    State  s                a
-- ScottyT { runS  :: State (ScottyState e m) a } 
--                    ExceptT  e               m                                            a
-- ActionT { runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a }
-- https://hackage.haskell.org/package/transformers-0.6.0.4/docs/Control-Monad-Trans-Except.html#t:ExceptT
-- Maybe     :: * -> *
-- Maybe Int :: *
-- Maybe ()  :: *

-- a is unit here
--                      e      m     a
application :: ScottyT Text ConfigM ()
application = do
  -- custom exception handler to avoid ugly default HTML
  defaultHandler handleEx

  -- ActionT is a MonadTrans (and a Monad)
  -- ActionT Text ConfigM
  get "/" $ do
    e <- asks environment
    json AppMsg {code = 200, message = e}
  get "/forbidden" $ do
    status status403
    json AppMsg {code = 403, message = "Forbidden"}
  get "/reco" $ do
    -- Take a Text value and parse it as a, or fail with a message.
    -- https://hackage.haskell.org/package/scotty-0.12/docs/Web-Scotty.html#v:parseParam
    -- rescue :: (ScottyError e, Monad m) => ActionT e m a -> (e -> ActionT e m a) -> ActionT e m a 
    -- Parsable a => Text/String -- const x == (\_ -> x)
    (p :: Either String String) <- rescue (Right <$> param "a") (\_ -> return $ Left "invalid params")
    case p of
      Left e  -> handleEx (pack e)
      Right m -> json AppMsg {code = 200, message = m}
  get "/word/:word" $ do
    c <- asks counts
    beam <- param "word"
    -- Expected: IO (Map Text Integer)
    --         -> ActionT Text ConfigM (IO (Map Text Integer))
    -- Actual: IO (Map Text Integer)
    --         -> ActionT Text IO (Map Text Integer)
    -- this is error
    -- let m' = (lift :: IO (Map Text Integer) -> ActionT Text ConfigM (IO (Map Text Integer))) (IORef.readIORef c :: IO (Map Text Integer))
    m <- liftIO (IORef.readIORef c :: IO (Map Text Integer))
    let (newM, times) = getTimes m $ Data.String.fromString beam
    liftIO (IORef.writeIORef c newM)
    json WordMsg {word = beam, times = fromInteger times}
-- https://github.com/scotty-web/scotty/blob/c36f35b89993f329b8ff08852c1535816375a926/Web/Scotty.hs#L278
  post "/" $ do
    conn <- asks conn
    expire <- asks expire
    b <- body
    let length = toInteger $ BL.length b
    let expire_time = floor $ retention (fromIntegral $ min_age expire) (fromIntegral $ max_age expire) (fromIntegral $ max_size expire) (fromIntegral length)
    if expire_time <= (min_age expire + 1)
      then handleEx "File too big"
    else do
      current <- liftIO getCurrentTime
      let expire_date = addUTCTime (secondsToNominalDiffTime $ fromIntegral expire_time) current
      uuid <- liftIO (UUID.randomIO :: IO UUID.UUID)
      liftIO $ Redis.runRedis conn $ do
        Redis.set (UUID.toASCIIBytes uuid) $ BL.toStrict b
        Redis.expire (UUID.toASCIIBytes uuid) expire_time
      json FileMsg {expire_time = show expire_date, uuid = UUID.toString uuid, file_size = length}
  -- TODO: get Recent 10 files
  -- Maybe I should have used MongoDB instead of Redis. Or Redis hash?
  get "/:uuid" $ do
    conn <- asks conn
    (uuid::String) <- param "uuid"
    let length = Prelude.length uuid
    -- TODO: fuzzy search
    -- https://stackoverflow.com/questions/5252099/redis-command-to-get-all-available-keys
    -- 7 digits are the Git default for a short SHA
    if length /= 36 then handle400 "Invalid uuid" else do
      (b:: Either Redis.Reply (Maybe BS.ByteString)) <- liftIO $ Redis.runRedis conn $ do
        Redis.get $ BSU.fromString uuid
      case b of
        Left e  -> handleEx (pack $ show e)
        Right Nothing -> handle404 "File not found"
        Right (Just bs) -> do
          text $ pack $ BSU.toString bs
    

  notFound $ do
    json AppMsg {code = 404, message = "Not found"}

main :: IO ()
main = do
  connection <- Redis.connect Redis.defaultConnectInfo
  c <- IORef.newIORef (Map.fromList [("", 0)] :: Map Text Integer)
  let config = Config { environment = "Development"
                      , counts = c
                      , conn = connection
                      , expire = Expire { min_age = 0
                                        , max_age = 30 * 24 * 60 * 60 -- 180 days in seconds
                                        , max_size = 1 * 1024 * 1024 -- 10 MB in bytes
                                        }}
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

-- http://dev.stephendiehl.com/hask/
-- https://github.com/haskell/mtl
-- https://stackoverflow.com/questions/9054731/avoiding-lift-with-monad-transformers
-- https://stackoverflow.com/questions/67186267/best-practices-with-monad-transformers-to-hide-or-not-to-hide-liftio
-- https://stackoverflow.com/questions/38626963/when-to-use-or-not-to-use-return-in-haskells-monad-expression
-- https://softwareengineering.stackexchange.com/questions/231136/why-does-a-monad-use-return-or-unit-rather-than-lift
-- https://stackoverflow.com/questions/23903031/lift-return-and-a-transformer-type-constructor
-- https://stackoverflow.com/questions/57374143/understanding-the-state-monad
-- https://stackoverflow.com/questions/5545517/difference-between-state-st-ioref-and-mvar
-- http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html
-- See https://github.com/scotty-web/scotty/blob/480ed62a17dbadd5128b67f9a448339e52930c1f/examples/exceptions.hs
-- See Large args section of https://typeclasses.com/featured/dollar
-- https://github.com/scotty-web/scotty/blob/master/examples/reader.hs

