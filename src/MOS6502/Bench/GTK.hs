module MOS6502.Bench.GTK (mkGUI) where

import MOS6502.Bench.Video

import Data.Sized.Unsigned (U4)
import Data.Sized.Matrix (Matrix)

import qualified Graphics.UI.Gtk as Gtk
import Control.Concurrent

mkGUI :: IO (Matrix FBAddr U4 -> IO ())
mkGUI = do
    imageMVar <- newEmptyMVar
    forkIO $ do
        Gtk.initGUI
        window <- Gtk.windowNew
        image <- Gtk.imageNew
        putMVar imageMVar image
        Gtk.set window [ Gtk.windowDefaultWidth Gtk.:= 300
                       , Gtk.windowDefaultHeight Gtk.:= 300
                       , Gtk.containerChild Gtk.:= image
                       , Gtk.containerBorderWidth Gtk.:= 10]
        Gtk.onDestroy window Gtk.mainQuit
        Gtk.widgetShowAll window
        Gtk.mainGUI
    return $ \mtx -> do
        image <- readMVar imageMVar
        Gtk.postGUIAsync $
          Gtk.imageSetFromPixbuf image =<< frameBufferToPixbuf mtx
