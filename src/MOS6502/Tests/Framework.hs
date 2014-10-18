{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MOS6502.Tests.Framework where

import MOS6502.Types
import MOS6502.Utils
import MOS6502.CPU

import Language.KansasLava hiding (Reg)
import Language.KansasLava.Signal (shallowMapS)
import Control.Monad.Writer
import Numeric (showHex)
import Data.Char (toUpper)
import Data.Monoid
import Data.Bits (shiftL, shiftR)
import Data.List (findIndex)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix, (!), Size, (//))
import qualified Data.Sized.Matrix as Matrix
import Test.QuickCheck hiding (Result(..))
import Test.QuickCheck.Property
-- import qualified Data.Traversable as T

import Debug.Trace

data Reg = A | X | Y | P

data Query a where
    Reg :: Reg -> Query Byte
    PC :: Query Addr
    Mem :: Addr -> Query Byte

data SomeQuery where
    SomeQuery :: Query a -> SomeQuery

regA :: Query Byte
regA = Reg A

regX :: Query Byte
regX = Reg X

regY :: Query Byte
regY = Reg Y

regPC :: Query Addr
regPC = PC

memZP :: Byte -> Query Byte
memZP = Mem . fromIntegral

mem :: Addr -> Query Byte
mem = Mem

statusFlags :: Query Byte
statusFlags = Reg P

data Phase = Before | After

data TestM (from :: Phase) (to :: Phase) (a :: *) where
    GetBefore :: Query a -> TestM Before Before a
    GetAfter :: Query a -> TestM After After a
    (:>>=) :: TestM from int a -> (a -> TestM int to b) -> TestM from to b
    Return :: a -> TestM from to a
    Execute :: Byte -> Int -> TestM Before After ()
    Assert :: String -> Bool -> TestM After After ()

execute :: Byte -> Int -> TestM Before After ()
execute = Execute

data Test = Op0 (TestM Before After ())
          | Op1 (Byte -> TestM Before After ())
          | Op2 (Addr -> TestM Before After ())

op0 :: TestM Before After () -> Test
op0 = Op0

op1 :: (Byte -> TestM Before After ()) -> Test
op1 = Op1

op2 :: (Addr -> TestM Before After ()) -> Test
op2 = Op2

assert :: String -> Bool -> TestM After After ()
assert = Assert

offset :: Addr -> Byte -> (Addr, Bool)
offset addr d = (addr', fromIntegral addr' < d)
  where
    addr' = addr + fromIntegral d

before :: Query a -> TestM Before Before a
before = GetBefore

after :: Query a -> TestM After After a
after = GetAfter

data InitialState = InitialState
    { arg1, arg2 :: Byte
    , initialA, initialX, initialY :: Byte
    , initialPC :: Addr
    , initialRAM :: Matrix Addr Byte
    }

instance Show InitialState where
    show InitialState{..} =
        unlines [ line "Arg1" arg1
                , line "Arg2" arg2
                , line "ArgAddr" argAddr
                , line "A" initialA
                , line "X" initialX
                , line "Y" initialY
                , line "PC" initialPC
                , line "B[ZP]" $ initialRAM ! fromIntegral arg1
                , line "B[ZP,X]" $ initialRAM ! fromIntegral (arg1 + initialX)
                , line "W[ZP,X]" wZPX
                , line "B[(ZP,X)]" $ initialRAM ! wZPX
                ]
      where
        line s v = unwords [s, "=", showHex_ v]

        argAddr :: Addr
        argAddr = toAddr arg1 arg2

        wZPX = toAddr (initialRAM ! fromIntegral (arg1 + initialX))
                      (initialRAM ! fromIntegral (arg1 + initialX + 1))

toAddr :: Byte -> Byte -> Addr
toAddr lo hi = fromIntegral hi `shiftL` 8 + fromIntegral lo

showHex_ :: (Show a, Integral a) => a -> String
showHex_ x = "$" <> (map toUpper $ showHex x "")

instance (Size ix) => Arbitrary (Unsigned ix) where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary InitialState where
    arbitrary = do
        arg1 <- arbitrary
        arg2 <- elements [0x00..0xEE]
        initialA <- arbitrary
        initialX <- arbitrary
        initialY <- arbitrary
        initialPC <- arbitrary `suchThat` (\x -> x > 0xF000 && x < 0xFF00)
        initialRAM <- arbitrary
        return InitialState{..}

instance forall ix a. (Size ix, Arbitrary a) => Arbitrary (Matrix ix a) where
    arbitrary = fmap Matrix.fromList $
                mapM (const arbitrary) (Matrix.all :: [ix])


nullRAM :: Matrix Addr Byte
nullRAM = constRAM 0

constRAM :: Byte -> Matrix Addr Byte
constRAM = Matrix.forAll . const

data Only a = Only{ getOnly :: Maybe a }

instance Monoid (Only a) where
    mempty = Only Nothing
    Only mx `mappend` Only mx' = case (mx, mx') of
        (Just _, Just _) -> error "Only"
        _ -> Only $ mx `mplus` mx'

runTest :: Test -> InitialState -> Result
runTest (Op0 test) is = runTestM test [] is
runTest (Op1 mkTest) is@InitialState{arg1} = runTestM (mkTest arg1) [arg1] is
runTest (Op2 mkTest) is@InitialState{arg1, arg2} = runTestM (mkTest addr) [arg1, arg2] is
  where
    addr = fromIntegral arg2 * 256 + fromIntegral arg1

whileJust :: [Maybe a] -> [a]
whileJust [] = []
whileJust (Nothing:_) = []
whileJust (Just x:xs) = x : whileJust xs

runTestM :: TestM Before After () -> [Byte] -> InitialState -> Result
runTestM test args InitialState{..} = if null failures then succeeded
                                      else failed{ reason = unlines failures }
  where
    run :: TestM from to a -> Writer (Only (Byte, Int), [(String, Bool)]) a
    run (GetBefore query) = return $ getBefore query
    run (GetAfter query) = return $ getAfter query
    run (m :>>= f) = run m >>= (run . f)
    run (Return x) = return x
    run (Execute opcode cycles) = tell (Only $ Just (opcode, cycles), mempty)
    run (Assert s b) = tell (mempty, [(s, b)])

    (Only (Just (opcode, _cycles)), messages) = execWriter (run test)
    failures = map fst . filter (not . snd) $ messages

    getBefore :: Query a -> a
    getBefore (Reg A) = initialA
    getBefore (Reg X) = initialX
    getBefore (Reg Y) = initialY
    getBefore (Mem addr) = initialRAM' ! addr

    getAfter :: Query a -> a
    getAfter (Reg A) = afterA
    getAfter (Reg X) = afterX
    getAfter (Reg Y) = afterY
    getAfter (Reg P) = afterP
    getAfter PC = afterPC
    getAfter (Mem addr) = afterRAM ! addr

    cpuIn :: CPUIn CLK
    cpuIn = CPUIn{..}

    initialRAM' = initialRAM // forced
      where
        forced = (initialPC, opcode) : zip [initialPC + 1..] (args ++ [0])

    cpuMemR = rom cpuMemA (Just . (initialRAM' !))
    cpuIRQ = low
    cpuNMI = low
    cpuWait = low

    numCycles = subtract 1 $
                fromMaybe (error "CPU didn't halt") $
                findIndex (== Halt) . whileJust . fromS $ cpuState

    listS :: (Rep a) => Signal CLK a -> [a]
    listS = catMaybes . take numCycles . fromS

    writes :: [(Addr, Byte)]
    writes = catMaybes $ listS pipe
      where
        pipe = packEnabled (isEnabled cpuMemW) $
               pack (cpuMemA, enabledVal cpuMemW)

    (afterA, afterX, afterY) = last $ listS $ pack (cpuA, cpuX, cpuY)
    (afterP, _afterSP, afterPC) = last $ listS $ pack (cpuP, cpuSP, cpuPC)

    afterRAM :: Matrix Addr Byte
    afterRAM = initialRAM' // writes

    (CPUOut{..}, CPUDebug{..}) = cpu' cpuInit cpuIn
      where
        cpuInit = CPUInit{ initA = initialA
                         , initX = initialX
                         , initY = initialY
                         , initPC = Just initialPC
                         }
