{-# LANGUAGE RecordWildCards #-}
module MOS6502.Tests.KlausDormann (test) where

import Distribution.TestSuite

import MOS6502.Types
import MOS6502.CPU
import MOS6502.Utils

import Language.KansasLava hiding (Reg)
import Language.KansasLava.Signal
import Control.Applicative ((<|>))
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
    start = 0x0400

    cpuInit = def{ initPC = Just start }
    (CPUOut{..}, debug@CPUDebug{..}) = cpu' cpuInit CPUIn{..}
    cpuWait = low

    romFun :: Addr -> Maybe Byte
    romFun a | a < start = Nothing
             | otherwise = Just . fromIntegral $ BS.index image $ fromIntegral a - fromIntegral start
    romR = rom cpuMemA romFun

    ram :: Seq (Addr -> Byte)
    ram = writeMemory $ forceDefined Nothing $
          packEnabled (isEnabled cpuMemW .&&. isRAM) $
          pack (cpuMemA, enabledVal cpuMemW)
    isRAM = cpuMemA .<. pureS start .||. cpuMemA .>=. 0xF000
    ramR = syncRead ram cpuMemA

    portAddr = 0xBFFC
    isPort = cpuMemA .==. pureS portAddr
    portW = packEnabled (isEnabled cpuMemW .&&. isPort) $ enabledVal cpuMemW
    (cpuIRQ, cpuNMI) = runRTL $ do
        fireIRQ <- newReg False
        fireNMI <- newReg False

        CASE [ match portW $ \x -> do
                    fireIRQ := x `testABit` 0
                    fireNMI := x `testABit` 1
             ]

        return (bitNot $ reg fireIRQ, bitNot $ reg fireNMI)
    portR = pureS 0

    cpuMemR = forceDefined 0x00 $
              memoryMapping [ (isRAM, ramR)
                            , (isPort, portR)
                            , (high, romR)
                            ]

suiteResult :: ByteString -> Result
suiteResult image = if trapPC `elem` targetPCs then Pass
                    else Fail $ showAddr trapPC
  where
    (pc, ram, CPUDebug{..}) = circuit image
    sig = pack (pc, syncRead ram 0x0203, pack (cpuState, cpuA, cpuP))
    pcs = [ pc | Just (pc, iSrc, (Fetch1, a, p)) <- fromS sig
               , let debug = [ showAddr pc
                             , showByte a
                             , showByte p
                             , if iSrc `testBit` 0 then "BRK" else "   "
                             , if iSrc `testBit` 1 then "IRQ" else "   "
                             , if iSrc `testBit` 2 then "NMI" else "   "
                             -- , show irqq
                             ]
                , _ <- trace (unwords debug) $ return ()
                ]

    trapPC = fst . head $ dropWhile (uncurry (/=)) $ zip pcs (tail pcs)
    targetPCs = [ 0x06fe -- NMOS 6502 NMI/BRK bug
                , 0x0700 -- Full success
                ]

showByte :: Byte -> String
showByte x = '$' : pad0 2 (showHex x "")

pad0 :: Int -> String -> String
pad0 n s = replicate (n - length s) '0' ++ s

showAddr :: Addr -> String
showAddr x = '$' : pad0 4 (showHex x "")

test :: IO Test
test = do
    image <- BS.readFile "testdata/KlausDormann-Interrupt.obj"
    let t = TestInstance{ run = return $ Finished $ suiteResult image
                        , name = "KlausDormann"
                        , tags = []
                        , options = []
                        , setOption = \_ _ -> Left "unsupported option"
                        }
    return $ Test t
