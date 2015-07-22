{-# LANGUAGE RecordWildCards #-}
module MOS6502.Tests.AllSuiteA (test) where

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

circuit :: ByteString -> (Signal CLK Addr, Signal CLK (Addr -> Byte), CPUDebug CLK)
circuit image = (cpuPC, ram, debug)
  where
    (CPUOut{..}, debug@CPUDebug{..}) = cpu CPUIn{..}
    cpuIRQ = high
    cpuNMI = high
    cpuWait = low

    romFun :: Addr -> Maybe Byte
    romFun a | a < 0x4000 = Nothing
             | otherwise = Just . fromIntegral $ BS.index image $ fromIntegral a - 0x4000
    romR = rom cpuMemA romFun

    ram :: Seq (Addr -> Byte)
    ram = writeMemory $ forceDefined Nothing $
          packEnabled (isEnabled cpuMemW .&&. isRAM) $
          pack (cpuMemA, enabledVal cpuMemW)
    isRAM = cpuMemA .<. 0x4000
    ramR = syncRead ram cpuMemA

    cpuMemR = forceDefined 0x00 $
              memoryMapping [(isRAM, ramR), (high, romR)]

suiteResult :: ByteString -> Result
suiteResult image = if targetValue == 0xFF then Pass
                    else Fail $ showByte targetValue
  where
    targetValue = head [ target
                       | Just (pc, target, (Fetch1, (a, (x, (y, p))))) <- fromS sig
                       , let debug = [ showAddr pc
                                     , "A ="
                                     , showByte a
                                     , "X ="
                                     , showByte x
                                     , "Y ="
                                     , showByte y
                                     , "P ="
                                     , showByte p
                                     ]
                       , trace (unwords debug) pc == finishPC
                       ]

    (pc, ram, CPUDebug{..}) = circuit image
    targetAddr = 0x0210
    finishPC = 0x45C0
    sig = pack (pc, syncRead ram targetAddr, pack (cpuState, pack (cpuA, pack (cpuX, pack (cpuY, cpuP)))))

showByte :: Byte -> String
showByte x = '$' : pad0 2 (showHex x "")

pad0 :: Int -> String -> String
pad0 n s = replicate (n - length s) '0' ++ s

showAddr :: Addr -> String
showAddr x = '$' : pad0 4 (showHex x "")

test :: IO Test
test = do
    image <- BS.readFile "testdata/AllSuiteA.obj"
    let t = TestInstance{ run = return $ Finished $ suiteResult image
                        , name = "AllSuiteA"
                        , tags = []
                        , options = []
                        , setOption = \_ _ -> Left "unsupported option"
                        }
    return $ Test t
