import System.IO
import System.Environment ( getArgs, getProgName )

import Control.Applicative
import Control.Monad.Reader

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Graphics.UI.Gtk as Gtk

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
runFile = runGtk
      <=< startWordProc
      <=< readWords

readWords :: FilePath -> IO [String]
readWords = fmap stringWords . L.readFile
    where stringWords = map L.unpack . L.words

-- | Set the contents of a Gtk Label to the current output of a 'Word.Proc'
labelCurrentWord :: (Gtk.LabelClass lbl) => lbl -> WordProc -> IO ()
labelCurrentWord lbl = setLabel <=< Word.output
  where
    setLabel :: String -> IO ()
    setLabel = Gtk.labelSetMarkup lbl . Gtk.markSpan attrs
    attrs = [Gtk.FontSize (Gtk.SizePoint 24)]


runGtk :: WordProc -> IO ()
runGtk wordProc = do
    Gtk.initGUI

    w <- Gtk.windowNew
    let close = False <$ liftIO Gtk.mainQuit
    w `Gtk.on` Gtk.deleteEvent  $ close
    w `Gtk.on` Gtk.destroyEvent $ close

    lbl <- Gtk.labelNew Nothing
    Gtk.containerAdd w lbl

    let updateWord = labelCurrentWord lbl wordProc

    Gtk.timeoutAdd (True <$ updateWord) 30

    Gtk.on w Gtk.keyPressEvent $ Gtk.tryEvent $ do
        k <- Gtk.eventKeyName
        liftIO (keyPress k wordProc)

    Gtk.widgetShowAll w

    Gtk.mainGUI


keyPress :: String -> WordProc -> IO ()
keyPress keyName =
    case keyName of
        "space"  -> prints "running: " <=< control (Running not)
        "k"      -> prints "wpm: "     <=< control (Rate (+5))
        "j"      -> prints "wpm: "     <=< control (Rate $ subtract 5)
        "Escape" -> \_ -> Gtk.mainQuit
        _        -> \_ -> return ()
  where
    prints s a = putStr s >> print a


printInstructions :: IO ()
printInstructions = mapM_ putStrLn
    [ "keys:"
    , "  spacebar       start/pause"
    , "  k              increase speed"
    , "  j              decrease speed"
    , "  escape         quit wordsurf"
    ]
