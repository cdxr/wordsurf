{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Word
( WordProc
, startWordProc
, output
, control
, Control (..)
) where


import Control.Lens

import Control.Monad.State

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async

import Control.Exception ( throwIO )

import Data.Tuple ( swap )

import Data.IORef
import Control.Concurrent.MVar


-- | Words per minute
type WPM = Int

type WordStream = [String]

stepWords :: (MonadState WordStream m) => m String
stepWords = state $ \ws ->
    case ws of
        [] -> ("", [])
        (w:wss) -> (w, wss)


data Control r where
    Running :: (Bool -> Bool) -> Control Bool
    Rate    :: (WPM  -> WPM)  -> Control WPM


data ProcState = ProcState
    { _rate          :: !WPM     -- ^ words per minute
    , _running       :: !Bool    -- ^ process paused when False
    , _timeSinceWord :: !Double  -- ^ Elapsed seconds since previous word
    , _wordStream    :: !WordStream
    } deriving (Show)

$(makeLenses ''ProcState)


newProcState :: WordStream -> ProcState 
newProcState = ProcState 300 False 0

-- | Calculate the delay until the next word, in milliseconds
interval :: ProcState -> Double
interval s = 60 / fromIntegral (_rate s)


runControl :: Control r -> State ProcState r
runControl (Running f) = do
    timeSinceWord .= 0
    running <%= f
runControl (Rate f) = rate <%= clampRate . f
  where
    clampRate = max 60 . min 600


data WordProc = WordProc
    (Async ())          -- ^ thread
    (MVar ProcState)    -- ^ procedure state
    (IORef String)      -- ^ currently displayed word


assertRunning :: Async a -> IO ()
assertRunning a = do
    r <- poll a
    case r of
        Just (Left e)  -> throwIO e
        Just (Right _) -> fail "WordProc has completed"
        Nothing -> return ()
    

control :: Control r -> WordProc -> IO r
control c (WordProc a s _) = do
    assertRunning a
    modifyMVar s $ return . swap . runState (runControl c)
    

output :: WordProc -> IO String
output (WordProc a _ o) = do
    assertRunning a
    readIORef o


startWordProc :: WordStream -> IO WordProc
startWordProc ws = do
    procState <- newMVar $ newProcState ws
    outputVar <- newIORef ""

    a <- async $ runWordProc procState outputVar

    return $ WordProc a procState outputVar


runWordProc :: MVar ProcState -> IORef String -> IO ()
runWordProc procVar wordVar = callEvery 30 $ \delta -> do
    newWord <- modifyMVar procVar $ \s -> return $
        if not (_running s)
          then (s, Nothing)
          else if realToFrac (_timeSinceWord s) < interval s
                 then (timeSinceWord +~ delta $ s, Nothing)
                 else let (w, s') = runState m s
                          m = do timeSinceWord .= 0
                                 zoom wordStream stepWords
                      in (s', Just w)
    case newWord of
        Nothing -> return ()
        Just w  -> atomicModifyIORef' wordVar $ \_ -> (w, ())


-- | Run a monadic action repeatedly at the given interval, in milliseconds
callEvery :: (MonadIO m) => Int -> (Double -> m a) -> m r
callEvery msDelay f = forever $ do
    liftIO $ threadDelay (msDelay * 1000)
    f (realToFrac msDelay / 1000)
