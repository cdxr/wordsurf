{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Word
( Proc (..)
, start
, ProcState
, rate
, running
, defaultConfig
, update
, updateIO
, changeRate
, toggle
, current
) where

import Control.Monad.IO.Class ()

import Control.Applicative

import Control.Lens

import Control.Monad.Reader
import Control.Monad.State

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async
import Control.Concurrent.STM

import Control.Monad.STM.Class

import Control.Mutate


-------------------------------------------------------------------------------
-- TODO
-- replace wordStream with more efficient implementation


data WordState = WordState
    { _wordStream    :: ![String]
    , _timeSinceWord :: !Int      -- | Elapsed time since previous word, in ms
    } deriving (Show)

makeLenses ''WordState


data ProcState = ProcState
    { _rate    :: !Double  -- | words per minute
    , _running :: !Bool    -- | process paused when False
    } deriving (Show)

makeLenses ''ProcState


defaultConfig :: ProcState 
defaultConfig = ProcState 300 False


data Proc = Proc
    { thread :: Async ()
    , public :: Edit STM ProcState
    , output :: STM String
    }

start :: [String] -> IO Proc
start = startProc . runWord
  where
    startProc spec = do
        v <- newTVarIO defaultConfig
        o <- newTVarIO ""
        a <- async $ spec v (write o)
        link a  -- rethrow any exceptions from the async thread
        return $ Proc a (edit v) (readVar o)

stepWords :: Word String
stepWords = step =<< use wordStream
  where
    step [] = return ""
    step (x:xs) = x <$ (wordStream .= xs)


type Word = ReaderT (TVar ProcState) (StateT WordState IO)

runWord :: [String] -> TVar ProcState -> Write STM String -> IO r
runWord !ws v out = evalStateT (runReaderT (wordLoop out) v) (WordState ws 0)

-- | Run a monadic action repeatedly at the given interval, in milliseconds
callEvery :: (MonadIO m) => Int -> (Int -> m a) -> m r
callEvery msDelay f = forever $ do
    liftIO $ threadDelay (msDelay * 1000)
    f msDelay

wordLoop :: Write STM String -> Word r
wordLoop out = callEvery 30 $ \delta ->
    step delta =<< liftSTM . readVar =<< ask
  where 
    step delta s
      | not (view running s) = timeSinceWord .= 0
      | otherwise = do
          t <- use timeSinceWord
          if realToFrac t < interval s
             then timeSinceWord += delta
             else do
                 liftSTM . writeVar out =<< stepWords
                 timeSinceWord .= 0


-- | Calculate the delay until the next word, in milliseconds
interval :: ProcState -> Double
interval = views rate (60 /)

-- | Modify the public state of the 'Proc'
update :: (ProcState -> ProcState) -> Proc -> STM ()
update f = flip editVar f . public

updateIO :: (ProcState -> ProcState) -> Proc -> IO ()
updateIO f = atomically . update f

changeRate :: (Double -> Double) -> Proc -> IO ()
changeRate f = updateIO $ rate %~ f

toggle :: Proc -> IO ()
toggle = updateIO $ running %~ not

current :: Proc -> IO String
current = atomically . output 
