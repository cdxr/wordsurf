import System.IO
import System.Environment ( getArgs, getProgName )

import Control.Applicative
import Control.Monad.Reader

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async

import Word


main = run =<< getArgs
  where
    run [file] = do
        printInstructions
        runFile file
    run _ = do
        pn <- getProgName
        putStrLn $ unwords ["usage:", pn, "TEXTFILE"]


runFile :: FilePath -> IO ()
runFile = runApp
      <=< startWordProc
      <=< readWords

readWords :: FilePath -> IO [String]
readWords = fmap stringWords . L.readFile
    where stringWords = map L.unpack . L.words


runApp :: WordProc -> IO ()
runApp = startGUI defaultConfig { tpPort = 10000 } . setup


setup :: WordProc -> Window -> UI ()
setup wordProc w = do
    set title "WordSurf" $ return w
    
    wordDisplay <- UI.span
    toggle <- set UI.text "start" UI.button

    getBody w #+
        [ column
            [ element wordDisplay
            , element toggle
            ]
        ]

    wordReader <- liftIO $ async $ forever $ do
        word <- readWord wordProc
        void $ atomic w $ runUI w $ set text word $ return wordDisplay
        threadDelay 30000

    body <- getBody w
    on UI.keydown body $ \key -> liftIO $ keyPress key wordProc

    on UI.click toggle $ \_ -> liftIO $
        prints "running: " =<< control (Running not) wordProc

    on UI.disconnect w $ \_ -> liftIO $ cancel wordReader
  where
    prints s a = putStr s >> print a


keyPress :: UI.KeyCode -> WordProc -> IO ()
keyPress keyCode =
    case toEnum keyCode of
        ' ' -> prints "running: " <=< control (Running not)
        'K' -> prints "wpm: "     <=< control (Rate (+5))
        'J' -> prints "wpm: "     <=< control (Rate $ subtract 5)
        _        -> \_ -> return ()
  where
    prints s a = putStr s >> print a


printInstructions :: IO ()
printInstructions = mapM_ putStrLn
    [ "keys:"
    , "  spacebar       start/pause"
    , "  k              increase speed"
    , "  j              decrease speed"
    ]
