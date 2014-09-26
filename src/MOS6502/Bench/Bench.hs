{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.Bench.Bench where

import MOS6502.Types
import MOS6502.CPU
-- import Utils (ramWithInit)

import Language.KansasLava
import Data.Bits
import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix, (!))
import qualified Data.Sized.Matrix as Matrix
import Data.List (transpose)

import qualified Graphics.UI.Gtk as Gtk
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Maybe (fromMaybe)
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Monad (forM_)
import Data.Word

type FBAddr = U10

-- dropInit :: (Rep a) => Seq a -> Seq a
-- dropInit = toS' . drop (fromIntegral (maxBound :: Addr)) . fromS

extractFB :: (Clock clk) => Signal clk (Addr -> Byte) -> [Matrix FBAddr Byte]
extractFB ram = map (fmap (fromMaybe 0) . Matrix.fromList) . transpose . map (fromS . asyncRead ram . pureS) $ [startAddr..endAddr]
  where
    startAddr = 0x200
    endAddr = startAddr + fromIntegral (maxBound :: FBAddr)

mkGUI :: IO (Matrix FBAddr Byte -> IO ())
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
        Gtk.postGUIAsync $ do
            pb <- allocaBytes (1024 * 3) $ \ptr -> do
                forM_ [minBound..maxBound :: FBAddr] $ \i -> do
                    let (r, g, b) = palette $ fromIntegral $ mtx!i
                        offset = fromIntegral i * 3
                    pokeElemOff ptr (offset + 0) (fromIntegral r)
                    pokeElemOff ptr (offset + 1) (fromIntegral g)
                    pokeElemOff ptr (offset + 2) (fromIntegral b)
                Gtk.pixbufNewFromData ptr Gtk.ColorspaceRgb False 8 width height (3 * width)
            pb' <- Gtk.pixbufScaleSimple pb (width * 8) (height * 8) Gtk.InterpNearest
            Gtk.imageSetFromPixbuf image pb'
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

demo :: IO ()
demo = do
    push <- mkGUI
    let step i = do
            print i
            getLine
            push $ bench !! i
    step 0
    step 1
    mapM_ step [10..20]

bench :: [Matrix FBAddr Byte]
bench = extractFB ram
  where
    (_cpuOut@CPUOut{..}, _cpuDebug) = cpu CPUIn{..}

    cpuIRQ :: Seq Bool
    cpuIRQ = high
    cpuNMI = high

    pipe = packEnabled (isEnabled cpuMemW) (pack (cpuMemA, enabledVal cpuMemW))
    ram = writeMemory pipe
    cpuWait = low
    -- (ram, cpuWait) = ramWithInit (+1) (const $ pureS 0) pipe
    ramR = syncRead ram cpuMemA

    -- One page of ROM, to be mapped to 0xFFxx
    romR = rom (unsigned cpuMemA) (Just . romPage)
    romPage :: Byte -> Byte
    romPage addr = case addr of
        -- LDA #$01
        0x00 -> 0xa9
        0x01 -> 0x01

        -- STA $0200
        0x02 -> 0x8d
        0x03 -> 0x00
        0x04 -> 0x02

        -- LDA #$05
        0x05 -> 0xa9
        0x06 -> 0x05

        -- STA $0201
        0x07 -> 0x8d
        0x08 -> 0x01
        0x09 -> 0x02

        -- LDA #$08
        0x0a -> 0xa9
        0x0b -> 0x08

        -- STA $0202
        0x0c -> 0x8d
        0x0d -> 0x02
        0x0e -> 0x02

        0xFC -> 0x00
        0xFD -> 0xff
        _ -> 0x00

    isROM = delay $ unsigned (cpuMemA `shiftR` 8) .==. pureS (0xFF :: Byte)

    cpuMemR = mux isROM (ramR, romR)
