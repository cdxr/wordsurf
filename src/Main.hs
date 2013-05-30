import System.IO
import System.Environment ( getArgs, getProgName )

import Control.Applicative
import Control.Monad.Reader

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Graphics.UI.Gtk as Gtk

import qualified Word


main = getArgs >>= run
  where
    run [file] = runFile file
    run _ = do
        pn <- getProgName
        putStrLn $ unwords ["usage:", pn, "TEXTFILE"]


runFile :: FilePath -> IO ()
runFile = runGtk
      <=< Word.start
      <=< readWords

readWords :: FilePath -> IO [String]
readWords = fmap stringWords . L.readFile
    where stringWords = map L.unpack . L.words

-- | @doEvery i m@ runs the event @m@ every @i@ milliseconds for the full
-- lifetime of the Gtk application
doEvery :: Int -> IO a -> IO Gtk.HandlerId
doEvery i m = Gtk.timeoutAdd (True <$ m) i

-- | Set the contents of a Gtk Label to the current word of a 'Word.Proc'
labelCurrentWord :: (Gtk.LabelClass lbl) => lbl -> Word.Proc -> IO ()
labelCurrentWord lbl = setLabel <=< Word.current
  where
    setLabel :: String -> IO ()
    setLabel = Gtk.labelSetMarkup lbl . Gtk.markSpan attrs
    attrs = [Gtk.FontSize (Gtk.SizePoint 24)]


runGtk :: Word.Proc -> IO ()
runGtk wordProc = do
    Gtk.initGUI

    w <- Gtk.windowNew
    let close = False <$ liftIO Gtk.mainQuit
    w `Gtk.on` Gtk.deleteEvent  $ close
    w `Gtk.on` Gtk.destroyEvent $ close

    lbl <- Gtk.labelNew Nothing
    Gtk.containerAdd w lbl

    doEvery 30 $ labelCurrentWord lbl wordProc

    Gtk.on w Gtk.keyPressEvent $ Gtk.tryEvent $ do
        k <- Gtk.eventKeyName
        liftIO (keyPress k wordProc)

    Gtk.widgetShowAll w
    Gtk.mainGUI


keyPress :: String -> Word.Proc -> IO ()
keyPress keyName =
    case keyName of
        "space"  -> Word.toggle
        "k"      -> Word.changeRate (+ 5)
        "j"      -> Word.changeRate (subtract 5)
        "Escape" -> \_ -> Gtk.mainQuit
        s        -> \_ -> print s
