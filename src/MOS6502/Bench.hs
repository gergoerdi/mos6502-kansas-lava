{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.Bench where

import MOS6502.Types
import MOS6502.CPU
-- import MOS6502.Opcodes
import MOS6502.Bench.Video (FBAddr)
import MOS6502.Bench.GTK

import Language.KansasLava
import Language.KansasLava.Signal (shallowMapS)
import Data.Bits
import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix, Size)
import qualified Data.Sized.Matrix as Matrix

import Data.List (transpose)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import Control.Applicative

memToMatrix :: (Clock clk, Rep a, Size a, Rep d) => Signal clk (a -> d) -> [Matrix a (Maybe d)]
memToMatrix ram = map Matrix.fromList . transpose . map (fromS . asyncRead ram . pureS) $ Matrix.all

{-
main :: IO ()
main = do
    fb <- demo'
    push <- mkGUI
    push $ fb !! 40000
    forever yield
-}

demo :: IO ()
demo = do
    fb <- demo'
    push <- mkGUI
    let step i = do
            print i
            getLine
            push $ fb !! i
    step 0
    step 1
    mapM_ step [10..]

demo' :: IO [Matrix FBAddr U4]
demo' = bench . programToROM 0xF000 <$> program

program :: IO BS.ByteString
-- program = BS.readFile "example/SoftSprites.obj"
program = BS.readFile "example/FullscreenLogo.obj"
-- program = BS.readFile "example/FillScreen.obj"

programToROM :: Addr -> BS.ByteString -> (Addr -> Byte)
programToROM startingAddr bs addr
  | addr == 0xFFFC = fromIntegral (startingAddr .&. 0xFF)
  | addr == 0xFFFD = fromIntegral (startingAddr `shiftR` 8)
  | offset < 0 = 0
  | offset >= BS.length bs = 0
  | otherwise = fromIntegral $ BS.index bs offset
  where
    offset = fromIntegral $ addr - startingAddr

forceDefined :: (Clock clk, Rep a) => a -> Signal clk a -> Signal clk a
forceDefined def = shallowMapS (fmap (optX . (<|> Just def) . unX))

bench :: (Addr -> Byte) -> [Matrix FBAddr U4]
bench romContents = map (fmap $ fromMaybe 0) $ memToMatrix vram
-- bench romContents = pack (cpuOp _cpuDebug, cpuState _cpuDebug, cpuPC _cpuDebug) :: Signal CLK (Opcode, State, Addr)
  where
    (_cpuOut@CPUOut{..}, _cpuDebug) = cpu CPUIn{..}

    cpuIRQ :: Seq Bool
    cpuIRQ = high
    cpuNMI = high

    mpipe :: Signal CLK (Pipe U14 Byte)
    mpipe = packEnabled (isEnabled cpuMemW .&. isRAM) $
            pack (unsigned cpuMemA, enabledVal cpuMemW)
    ram = writeMemory mpipe
    cpuWait = low

    ramR = syncRead ram (unsigned cpuMemA)
    ramR' = forceDefined 0 ramR

    -- 1K of video RAM mapped from 0x0200
    isVideo = 0x0200 .<=. cpuMemA .&&. cpuMemA .<. 0x0400

    vpipe :: Signal CLK (Pipe FBAddr U4)
    vpipe = packEnabled (isEnabled cpuMemW .&&. isVideo) $
            pack (unsigned (cpuMemA - 0x0200), unsigned $ enabledVal cpuMemW)
    vram = writeMemory vpipe

    romR = rom cpuMemA (Just . romContents)

    -- 16K of RAM mapped from 0x000
    isRAM = cpuMemA .<. 0x4000

    -- One page of ROM is mapped from 0xF000
    isROM = delay $ cpuMemA .>=. 0xF000

    cpuMemR = mux isROM (ramR', romR)
