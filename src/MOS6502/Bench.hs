{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.Bench where

import MOS6502.Types
import MOS6502.CPU
import MOS6502.Opcodes
import MOS6502.Bench.Video (FBAddr)
import MOS6502.Bench.GTK

import Language.KansasLava
import Language.KansasLava.Signal (shallowMapS)
import Data.Bits
import Data.Sized.Matrix (Matrix)
import qualified Data.Sized.Matrix as Matrix

import Data.List (transpose)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import Control.Applicative
import Control.Monad

extractFB :: (Clock clk) => Signal clk (Addr -> Byte) -> [Matrix FBAddr Byte]
extractFB ram = map (fmap (fromMaybe 0) . Matrix.fromList) . transpose . map (fromS . asyncRead ram . pureS) $ [startAddr..endAddr]
  where
    startAddr = 0x200
    endAddr = startAddr + fromIntegral (maxBound :: FBAddr)

{-
main :: IO ()
main = do
    fb <- demo'
    push <- mkGUI
    push $ fb !! 40000
    forever yield
-}

{-
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
-}

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

bench :: (Addr -> Byte) -> [Matrix FBAddr Byte]
bench romContents = extractFB ram
-- bench romContents = pack (cpuOp _cpuDebug, cpuState _cpuDebug, cpuPC _cpuDebug) :: Signal CLK (Opcode, State, Addr)
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
    ramR' = forceDefined 0 ramR

    romR = rom cpuMemA (Just . romContents)

    -- One page of ROM is mapped to 0xFFxx
    isROM = delay $ unsigned (cpuMemA `shiftR` 8) .>=. pureS (0xF0 :: Byte)

    cpuMemR = mux isROM (ramR', romR)
