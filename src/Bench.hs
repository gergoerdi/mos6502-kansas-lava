{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Bench where

import Types
import MOS6502
-- import Utils (ramWithInit)

import Language.KansasLava
import Data.Bits
import Data.Sized.Matrix (Matrix)
import qualified Data.Sized.Matrix as Matrix
import Data.List (transpose)

-- dropInit :: (Rep a) => Seq a -> Seq a
-- dropInit = toS' . drop (fromIntegral (maxBound :: Addr)) . fromS

extractFB :: (Clock clk) => Signal clk (Addr -> Byte) -> [Matrix Byte (Maybe Byte)]
extractFB ram = map Matrix.fromList . transpose . map (fromS . asyncRead ram . pureS) $ [startAddr..startAddr + size]
  where
    startAddr = 0x200
    size = 0xff

-- bench :: (CPUOut CLK, CPUDebug CLK)
-- bench = (_cpuOut, _cpuDebug)
-- bench :: Seq (Addr, Byte, Byte)
-- bench = pack (cpuMemA, ramR, romR)
-- bench :: Seq ((Addr, Byte), State)
-- bench = pack (pack (cpuMemA, cpuMemR), cpuState _cpuDebug)
-- bench :: Seq Byte
-- bench = fb
-- bench :: Seq (State, Addr)
-- bench = pack (cpuState _cpuDebug, cpuPC _cpuDebug)
bench = fb
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

    -- fb = syncRead ram (pureS 0x0200)
    fb = ram

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
