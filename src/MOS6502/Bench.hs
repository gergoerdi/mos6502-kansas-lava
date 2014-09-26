{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.Bench where

import MOS6502.Types
import MOS6502.CPU
import MOS6502.Bench.Video
-- import Utils (ramWithInit)

import Language.KansasLava
import Data.Bits
import Data.Sized.Matrix (Matrix)
import qualified Data.Sized.Matrix as Matrix
import Data.List (transpose)

import Data.Maybe (fromMaybe)

-- dropInit :: (Rep a) => Seq a -> Seq a
-- dropInit = toS' . drop (fromIntegral (maxBound :: Addr)) . fromS

extractFB :: (Clock clk) => Signal clk (Addr -> Byte) -> [Matrix FBAddr Byte]
extractFB ram = map (fmap (fromMaybe 0) . Matrix.fromList) . transpose . map (fromS . asyncRead ram . pureS) $ [startAddr..endAddr]
  where
    startAddr = 0x200
    endAddr = startAddr + fromIntegral (maxBound :: FBAddr)

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
