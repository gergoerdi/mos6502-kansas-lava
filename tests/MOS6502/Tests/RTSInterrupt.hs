{-# LANGUAGE RecordWildCards #-}
module MOS6502.Tests.RTSInterrupt (test) where

import Distribution.TestSuite

import MOS6502.Types
import MOS6502.CPU
import MOS6502.Utils

import Language.KansasLava hiding (Reg)
import Language.KansasLava.Signal
import Control.Applicative ((<|>))
import Numeric (showHex)
import Debug.Trace

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

forceDefined :: (Clock clk, Rep a) => a -> Signal clk a -> Signal clk a
forceDefined def = shallowMapS (fmap (optX . (<|> Just def) . unX))

circuit :: ByteString -> (Signal CLK Addr, Signal CLK Byte, CPUDebug CLK)
circuit image = (cpuPC, forceDefined 0x00 $ syncRead ram targetAddr, debug)
  where
    (CPUOut{..}, debug@CPUDebug{..}) = cpu CPUIn{..}
    cpuIRQ = toS $ replicate 10 True ++ replicate 10 False ++ repeat True
    cpuNMI = high
    cpuWait = low

    romFun :: Addr -> Maybe Byte
    romFun a | a < 0xF000 = Nothing
             | otherwise = Just . fromIntegral $ BS.index image $ fromIntegral a - 0xF000
    romR = rom cpuMemA romFun

    ram :: Seq (Addr -> Byte)
    ram = writeMemory $ forceDefined Nothing $
          packEnabled (isEnabled cpuMemW .&&. isRAM) $
          pack (cpuMemA, enabledVal cpuMemW)
    isRAM = cpuMemA .<. 0xF000
    ramR = syncRead ram cpuMemA

    cpuMemR = forceDefined 0x00 $
              memoryMapping [(isRAM, ramR), (high, romR)]

    targetAddr = 0x1234

suiteResult :: ByteString -> Result
suiteResult image = if targetValue == 0xEA then Pass
                    else Fail $ unwords ["'Interrupt while RTS' failed:", showByte targetValue]
  where
    targetValue = head [ target
                       | Just (pc, target, (Fetch1, (a, sp))) <- fromS sig
                       , let debug = [ showAddr pc
                                     , "A ="
                                     , showByte a
                                     , "SP ="
                                     , showByte sp
                                     ]
                       , trace (unwords debug) $ pc == finishPC
                       ]

    (pc, targetVal, CPUDebug{..}) = circuit image
    finishPC = 0xF008

    sig = pack (pc, targetVal, pack (cpuState, pack (cpuA, cpuSP)))

showByte :: Byte -> String
showByte x = '$' : pad0 2 (showHex x "")

pad0 :: Int -> String -> String
pad0 n s = replicate (n - length s) '0' ++ s

showAddr :: Addr -> String
showAddr x = '$' : pad0 4 (showHex x "")

test :: IO Test
test = do
    image <- BS.readFile "testdata/RTSInterrupt.obj"
    let t = TestInstance{ run = return $ Finished $ suiteResult image
                        , name = "RTSInterrupt"
                        , tags = []
                        , options = []
                        , setOption = \_ _ -> Left "unsupported option"
                        }
    return $ Test t
