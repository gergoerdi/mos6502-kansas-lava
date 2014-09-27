{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import MOS6502.Bench

import MOS6502.Types
import MOS6502.Bench.Video
import MOS6502.Bench.GTK

import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix, (!))

import qualified Graphics.UI.Gtk as Gtk
import Control.Concurrent
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Monad (forM_)
import Data.Word

import Control.Monad
import Control.Concurrent

main :: IO ()
main = do
    fb <- demo'
    push <- mkGUI
    push $ fb !! 40000
    forever yield

{-
main :: IO ()
main = do
    fb <- demo'
    let mtx = fb !! 50000

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
-}
