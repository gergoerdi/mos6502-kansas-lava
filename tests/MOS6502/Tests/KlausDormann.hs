{-# LANGUAGE RecordWildCards #-}
module MOS6502.Tests.KlausDormann (test) where

import Distribution.TestSuite

import MOS6502.Types
import MOS6502.CPU
import MOS6502.Utils

import Language.KansasLava hiding (Reg)
import Language.KansasLava.Signal
import Control.Applicative ((<|>), (<*>), (<$>))
import Numeric (showHex)
import Data.Default
import Data.Bits
import Debug.Trace

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

forceDefined :: (Clock clk, Rep a) => a -> Signal clk a -> Signal clk a
forceDefined def = shallowMapS (fmap (optX . (<|> Just def) . unX))

circuit :: ByteString -> (Signal CLK Addr, Signal CLK (Addr -> Byte), CPUDebug CLK)
circuit image = (cpuPC, ram, debug)
  where
    start = 0x1000

    cpuInit = def{ initPC = Just start }
    (CPUOut{..}, debug@CPUDebug{..}) = cpu' cpuInit CPUIn{..}
    cpuWait = low
    cpuIRQ = high
    cpuNMI = high

    romFun :: Addr -> Maybe Byte
    romFun a | a < start = Nothing
             | otherwise = Just . fromIntegral $ BS.index image $ fromIntegral a - fromIntegral start
    romR = rom cpuMemA romFun

    ram :: Seq (Addr -> Byte)
    ram = writeMemory $ forceDefined Nothing $
          packEnabled (isEnabled cpuMemW) $
          pack (cpuMemA, enabledVal cpuMemW)
    ramR = initWith romR $ syncRead ram cpuMemA

    initWith sROM sRAM = mkShallowS . fmap optX $
                         (<|>) <$> (unX <$> shallowS sRAM) <*> (unX <$> shallowS sROM)

    cpuMemR = forceDefined 0x00 ramR

suiteResult :: ByteString -> Result
suiteResult image = if trapPC == targetPC then Pass
                    else Fail $ showAddr trapPC
  where
    (pc, _ram, CPUDebug{..}) = circuit image
    sig = pack (pc, pack (cpuState, pack (cpuA, cpuX, cpuY), cpuP))
    pcs = [ pc | Just (pc, (Fetch1, (a, x, y), p)) <- fromS sig
               , let debug = [ showAddr pc
                             , showByte a
                             , showByte x
                             , showByte y
                             , showByte p
                             ]
                , _ <- trace (unwords debug) $ return ()
                ]

    trapPC = fst . head $ dropWhile (uncurry (/=)) $ zip pcs (tail pcs)
    targetPC = 0x3B31

showByte :: Byte -> String
showByte x = '$' : pad0 2 (showHex x "")

pad0 :: Int -> String -> String
pad0 n s = replicate (n - length s) '0' ++ s

showAddr :: Addr -> String
showAddr x = '$' : pad0 4 (showHex x "")

test :: IO Test
test = do
    image <- BS.readFile "testdata/KlausDormann.obj"
    let t = TestInstance{ run = return $ Finished $ suiteResult image
                        , name = "KlausDormann"
                        , tags = []
                        , options = []
                        , setOption = \_ _ -> Left "unsupported option"
                        }
    return $ Test t
