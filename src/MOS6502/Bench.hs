{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.Bench where

import MOS6502.Types
import MOS6502.CPU
import MOS6502.Bench.Video (FBAddr)
import MOS6502.Bench.GTK

import Language.KansasLava
import Data.Bits
import Data.Sized.Matrix (Matrix)
import qualified Data.Sized.Matrix as Matrix

import Data.List (transpose)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import Control.Applicative

extractFB :: (Clock clk) => Signal clk (Addr -> Byte) -> [Matrix FBAddr Byte]
extractFB ram = map (fmap (fromMaybe 0) . Matrix.fromList) . transpose . map (fromS . asyncRead ram . pureS) $ [startAddr..endAddr]
  where
    startAddr = 0x200
    endAddr = startAddr + fromIntegral (maxBound :: FBAddr)

demo :: IO ()
demo = do
    rom <- programToROM 0xFF00 <$> program
    push <- mkGUI
    let fb = bench rom
    let step i = do
            print i
            getLine
            push $ fb !! i
    step 0
    step 1
    mapM_ step [10..]

program :: IO BS.ByteString
program = BS.readFile "example/FillScreen.obj"

programToROM :: Addr -> BS.ByteString -> (Addr -> Byte)
programToROM startingAddr bs addr
  | addr == 0xFFFC = fromIntegral (startingAddr .&. 0xFF)
  | addr == 0xFFFD = fromIntegral (startingAddr `shiftR` 8)
  | offset < 0 = 0
  | offset >= BS.length bs = 0
  | otherwise = fromIntegral $ BS.index bs offset
  where
    offset = fromIntegral $ addr - startingAddr

bench :: (Addr -> Byte) -> [Matrix FBAddr Byte]
bench romContents = extractFB ram
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

    romR = rom cpuMemA (Just . romContents)

    -- One page of ROM is mapped to 0xFFxx
    isROM = delay $ unsigned (cpuMemA `shiftR` 8) .==. pureS (0xFF :: Byte)

    cpuMemR = mux isROM (ramR, romR)
