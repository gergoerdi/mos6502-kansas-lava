{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MOS6502.Tests.Framework where

import MOS6502.Types
import MOS6502.Utils
import MOS6502.CPU

import Language.KansasLava hiding (Reg)
import Language.KansasLava.Signal (shallowMapS)
import Control.Monad.Writer
import Data.Monoid
import Data.List (findIndex)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix, (!), Size, (//))
import qualified Data.Sized.Matrix as Matrix
import Test.QuickCheck
-- import qualified Data.Traversable as T

import Debug.Trace

data Reg = A | X | Y

data Query a where
    Reg :: Reg -> Query Byte
    PC :: Query Addr
    ZP :: Byte -> Query Byte
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
memZP = ZP

mem :: Addr -> Query Byte
mem = Mem

derefZP :: Byte -> Query Addr
derefZP = undefined

data Phase = Before | After

data TestM (from :: Phase) (to :: Phase) (a :: *) where
    GetBefore :: Query a -> TestM Before Before a
    GetAfter :: Query a -> TestM After After a
    (:>>=) :: TestM from int a -> (a -> TestM int to b) -> TestM from to b
    Return :: a -> TestM from to a
    Execute :: Byte -> Int -> TestM Before After ()
    Assert :: Bool -> TestM After After ()

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

assert :: Bool -> TestM After After ()
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
    { initialA, initialX, initialY :: Byte
    , initialPC :: Addr
    , initialZP :: Matrix Byte Byte
    }
    deriving Show

instance (Size ix) => Arbitrary (Unsigned ix) where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary InitialState where
    arbitrary = do
        initialA <- arbitrary
        initialX <- arbitrary
        initialY <- arbitrary
        initialPC <- arbitrary `suchThat` (\x -> x > 0x1000 && x < 0xFF00)
        initialZP <- arbitrary
        return InitialState{..}

instance forall ix a. (Size ix, Arbitrary a) => Arbitrary (Matrix ix a) where
    arbitrary = liftM Matrix.fromList $
                mapM (const arbitrary) (Matrix.all :: [ix])


nullZP :: Matrix Byte Byte
nullZP = constZP 0

constZP :: Byte -> Matrix Byte Byte
constZP = Matrix.forAll . const

data Only a = Only{ getOnly :: Maybe a }

instance Monoid (Only a) where
    mempty = Only Nothing
    Only mx `mappend` Only mx' = case (mx, mx') of
        (Just _, Just _) -> error "Only"
        _ -> Only $ mx `mplus` mx'

runTest :: Test -> Byte -> Byte -> InitialState -> Bool
runTest (Op0 test) _arg1 _arg2 = runTestM test []
runTest (Op1 mkTest) arg1 _arg2 = runTestM (mkTest arg1) [arg1]
runTest (Op2 mkTest) arg1 arg2 = runTestM (mkTest addr) [arg1, arg2]
  where
    addr = fromIntegral arg2 * 256 + fromIntegral arg1

whileJust :: [Maybe a] -> [a]
whileJust [] = []
whileJust (Nothing:_) = []
whileJust (Just x:xs) = x : whileJust xs

runTestM :: TestM Before After () -> [Byte] -> InitialState -> Bool
runTestM test args InitialState{..} = pass
  where
    run :: TestM from to a -> Writer (Only (Byte, Int), All) a
    run (GetBefore query) = return $ getBefore query
    run (GetAfter query) = return $ getAfter query
    run (m :>>= f) = run m >>= (run . f)
    run (Return x) = return x
    run (Execute opcode cycles) = tell (Only $ Just (opcode, cycles), mempty)
    run (Assert b) = tell (mempty, All b)

    (Only (Just (opcode, _cycles)), All pass) = execWriter (run test)

    getBefore :: Query a -> a
    getBefore (Reg A) = initialA
    getBefore (Reg X) = initialX
    getBefore (Reg Y) = initialY
    getBefore (ZP zp) = initialZP ! zp

    getAfter :: Query a -> a
    getAfter (Reg A) = afterA
    getAfter (Reg X) = afterX
    getAfter (Reg Y) = afterY
    getAfter PC = afterPC
    getAfter (ZP zp) = afterZP ! zp

    cpuIn :: CPUIn CLK
    cpuIn = CPUIn{..}

    progROM addr | offset == 0 = opcode
                 | offset <= numArgs = args !! (offset - 1)
                 | otherwise = 0
      where
        offset = fromIntegral $ addr - initialPC
        numArgs = length args

    zpROM addr = initialZP ! fromIntegral addr

    cpuMemR = memoryMapping 0 $
              [ (cpuMemA .<=. 0xFF, rom cpuMemA (Just . zpROM))
              , (high, rom cpuMemA (Just . progROM))
              ]
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
    afterPC = last $ listS cpuPC

    afterZP :: Matrix Byte Byte
    afterZP = initialZP // mapMaybe toZP writes
      where
        toZP (addr, byte) = do
            guard $ addr <= 0xFF
            return (fromIntegral addr, byte)

    (CPUOut{..}, CPUDebug{..}) = cpu' cpuInit cpuIn
      where
        cpuInit = CPUInit{ initA = initialA
                         , initX = initialX
                         , initY = initialY
                         , initPC = Just initialPC
                         }
