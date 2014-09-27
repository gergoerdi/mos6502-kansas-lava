module MOS6502.Bench.Video (FBAddr, frameBufferToPixbuf) where

import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix, (!))

import qualified Graphics.UI.Gtk as Gtk
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Monad (forM_)
import Data.Word

type FBAddr = U10

frameBufferToPixbuf :: Matrix FBAddr U4 -> IO Gtk.Pixbuf
frameBufferToPixbuf mtx = do
    pb <- allocaBytes (1024 * 3) $ \ptr -> do
        forM_ [minBound..maxBound :: FBAddr] $ \i -> do
            let (r, g, b) = palette $ mtx!i
                offset = fromIntegral i * 3
            pokeElemOff ptr (offset + 0) (fromIntegral r)
            pokeElemOff ptr (offset + 1) (fromIntegral g)
            pokeElemOff ptr (offset + 2) (fromIntegral b)
        Gtk.pixbufNewFromData ptr Gtk.ColorspaceRgb False 8 width height (3 * width)
    Gtk.pixbufScaleSimple pb (width * 8) (height * 8) Gtk.InterpNearest
  where
    width = 32
    height = 32

palette :: U4 -> (Word8, Word8, Word8)
palette 0x0 = (0x00, 0x00, 0x00) -- Black
palette 0x1 = (0xff, 0xff, 0xff) -- White
palette 0x2 = (0x88, 0x00, 0x00) -- Red
palette 0x3 = (0xaa, 0xff, 0xee) -- Cyan
palette 0x4 = (0xcc, 0x44, 0xcc) -- Purple
palette 0x5 = (0x00, 0xcc, 0x55) -- Green
palette 0x6 = (0x00, 0x00, 0xaa) -- Blue
palette 0x7 = (0xee, 0xee, 0x77) -- Yellow
palette 0x8 = (0xdd, 0x88, 0x55) -- Orange
palette 0x9 = (0x66, 0x44, 0x00) -- Brown
palette 0xa = (0xff, 0x77, 0x77) -- Light red
palette 0xb = (0x33, 0x33, 0x33) -- Dark gray
palette 0xc = (0x77, 0x77, 0x77) -- Gray
palette 0xd = (0xaa, 0xff, 0x66) -- Light green
palette 0xe = (0x00, 0x88, 0xff) -- Light blue
palette 0xf = (0xbb, 0xbb, 0xbb) -- Light gray
