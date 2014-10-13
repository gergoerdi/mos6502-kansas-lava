{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import MOS6502.Bench
import MOS6502.Bench.Video

import qualified Graphics.UI.Gtk as Gtk
import Control.Concurrent
import System.Environment (getArgs)
import qualified Data.ByteString as BS
import Control.Applicative

-- main :: IO ()
-- main = do
--     fb <- demo'
--     push <- mkGUI
--     push $ fb !! 40000
--     forever yield

main :: IO ()
main = do
    args <- getArgs
    (fileName, steps) <- case args of
        [fileName] -> return (fileName, 5000)
        [fileName, s] -> return (fileName, read s)
        _ -> error "Missing filename"

    rom <- programToROM 0xF000 <$> BS.readFile fileName
    let mtxs = benchVideo rom
        mtx = mtxs !! steps

    Gtk.initGUI
    window <- Gtk.windowNew
    image <- Gtk.imageNew
    Gtk.set window [ Gtk.windowDefaultWidth Gtk.:= 300
                   , Gtk.windowDefaultHeight Gtk.:= 300
                   , Gtk.containerChild Gtk.:= image
                   , Gtk.containerBorderWidth Gtk.:= 10]
    Gtk.onDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    forkIO $ do
        fb <- frameBufferToPixbuf mtx
        Gtk.postGUISync $ Gtk.imageSetFromPixbuf image fb
    Gtk.mainGUI
