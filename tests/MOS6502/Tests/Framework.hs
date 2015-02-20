{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MOS6502.Tests.Framework where

import MOS6502.Types
import MOS6502.CPU

import Language.KansasLava hiding (Reg)
import Control.Monad.Writer
import Numeric (showHex)
import Data.Char (toUpper)
import Data.Bits (shiftL, shiftR)
-- import Data.List (findIndex)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix, (!), Size, (//))
import qualified Data.Sized.Matrix as Matrix

import Control.Applicative.Free
import Data.Functor.Coyoneda
import Control.Monad.RWS
import Control.Monad.Identity
import Control.Applicative

import Test.QuickCheck hiding (Result(..))
import Test.QuickCheck.Property

data Reg = A | X | Y | P | SP deriving Show

data Query a where
    Reg :: Reg -> Query Byte
    Mem :: Obs Addr -> Query Byte
    PC :: Query Addr

instance Show (Query a) where
    show (Reg r) = unwords ["Reg", show r]
    show (Mem _) = "Mem"
    show PC = "PC"

type Step = Int

data Obs' a = Query Step (Coyoneda Query a)
newtype Obs a = Obs{ unObs :: Ap Obs' a }
              deriving (Functor, Applicative)

instance (Num a) => Num (Obs a) where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = liftA abs
    signum = liftA signum

data Check = CheckTiming Step (Obs Int)
           | CheckAssertion String (Obs Bool)

regA :: Query Byte
regA = Reg A

regX :: Query Byte
regX = Reg X

regY :: Query Byte
regY = Reg Y

regPC :: Query Addr
regPC = PC

regSP :: Query Byte
regSP = Reg SP

memZP :: Obs Byte -> Query Byte
memZP = Mem . fmap fromIntegral

mem :: Obs Addr -> Query Byte
mem = Mem

statusFlags :: Query Byte
statusFlags = Reg P

newtype TestM a = TestM{ unTestM :: RWS () ([[Byte]], [Check]) Int a }
                deriving (Functor, Applicative, Monad)

data Test = Op0 String (TestM ())
          | Op1 String (Byte -> TestM ())
          | Op2 String (Addr -> TestM ())

execute :: [Byte] -> Obs Int -> TestM ()
execute bytes cycles = TestM $ do
    step <- get
    tell ([bytes], [CheckTiming step cycles])
    modify succ

execute0 :: Byte -> Obs Int -> TestM ()
execute0 opcode = execute [opcode]

execute1 :: Byte -> Byte -> Obs Int -> TestM ()
execute1 opcode arg = execute [opcode, arg]

execute2 :: Byte -> Addr -> Obs Int -> TestM ()
execute2 opcode addr = execute [opcode, lo, hi]
  where
    (lo, hi) = splitAddr addr

splitAddr :: Addr -> (Byte, Byte)
splitAddr addr = (fromIntegral addr, fromIntegral (addr `shiftR` 8))

testLabel :: Test -> String
testLabel (Op0 lab _) = lab
testLabel (Op1 lab _) = lab
testLabel (Op2 lab _) = lab

assert :: String -> Obs Bool -> TestM ()
assert s b = TestM $ do
    tell (mempty, [CheckAssertion s b])

offset :: Obs Addr -> Obs Byte -> (Obs Addr, Obs Bool)
offset addr d = let res = offset' <$> addr <*> d
                in (fst <$> res, snd <$> res)
  where
    offset' addr d = let addr' = addr + fromIntegral d
                     in (addr', fromIntegral addr' < d)

assertEq :: (Eq a, Show a) => String -> Obs a -> Obs a -> TestM ()
assertEq s x y = assert s ((==) <$> x <*> y)

observe :: Query a -> TestM (Obs a)
observe q = TestM $ do
    step <- get
    return $ Obs . liftAp . Query step . liftCoyoneda $ q

op0 :: String -> TestM () -> Test
op0 = Op0

op1 :: String -> (Byte -> TestM ()) -> Test
op1 = Op1

op2 :: String -> (Addr -> TestM ()) -> Test
op2 = Op2

data InitialState = InitialState
    { arg1, arg2 :: Byte
    , initialA, initialX, initialY, initialFlags :: Byte
    , initialPC :: Addr
    , initialSP :: Byte
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
                , line "SP" initialSP
                , line "Flags" initialFlags
                , line "B[@@]" $ byteAt $ fromIntegral arg1
                , line "B[@@,X]" $ byteAt $ fromIntegral (arg1 + initialX)
                , line "W[@@,X]" wZPX
                , line "B[(@@,X)]" $ byteAt wZPX
                , line "B[(@@),Y]" $ byteAt (wZP + fromIntegral initialY)
                , line "B[@@@@]" $ byteAt argAddr
                , line "W[@@@@]" $ wordAt argAddr
                , line "W[(@@@@)]" $ wordAt (wordAt argAddr)
                , line "B[top]" $ byteAt (0x100 + (fromIntegral $ initialSP + 1))
                ]
      where
        line s v = unwords [s, "=", showHex_ v]

        argAddr :: Addr
        argAddr = toAddr arg1 arg2

        byteAt addr = initialRAM ! addr
        wordAt addr = toAddr (byteAt addr) (byteAt addr')
          where
            (lo, hi) = (fromIntegral addr, fromIntegral (addr `shiftR` 8))
            addr' = toAddr (lo + 1) hi

        addrAt zp =  toAddr (byteAt $ fromIntegral zp)
                            (byteAt $ fromIntegral $ succ zp)

        wZP = addrAt arg1
        wZPX = addrAt (arg1 + initialX)

toAddr :: Byte -> Byte -> Addr
toAddr lo hi = fromIntegral hi `shiftL` 8 + fromIntegral lo

showHex_ :: (Show a, Integral a) => a -> String
showHex_ x = "$" <> (padLeft 2 '0' $ map toUpper $ showHex x "")

padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length xs) x ++ xs

instance (Size ix) => Arbitrary (Unsigned ix) where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary InitialState where
    arbitrary = do
        arg1 <- arbitrary
        arg2 <- elements [0x00..0xEE]
        initialA <- arbitrary
        initialX <- arbitrary
        initialY <- arbitrary
        initialFlags <- arbitrary
        initialPC <- arbitrary `suchThat` (\x -> x > 0xF000 && x < 0xFF00)
        initialSP <- arbitrary `suchThat` (> 0xE0)
        initialRAM <- arbitrary
        return InitialState{..}

instance forall ix a. (Size ix, Arbitrary a) => Arbitrary (Matrix ix a) where
    arbitrary = fmap Matrix.fromList $
                mapM (const arbitrary) (Matrix.all :: [ix])


nullRAM :: Matrix Addr Byte
nullRAM = constRAM 0

constRAM :: Byte -> Matrix Addr Byte
constRAM = Matrix.forAll . const

runTest :: Test -> InitialState -> Result
runTest (Op0 _ test) is = runTestM test is
runTest (Op1 _ mkTest) is@InitialState{arg1} = runTestM (mkTest arg1) is
runTest (Op2 _ mkTest) is@InitialState{arg1, arg2} = runTestM (mkTest addr) is
  where
    addr = fromIntegral arg2 * 256 + fromIntegral arg1

whileJust :: [Maybe a] -> [a]
whileJust [] = []
whileJust (Nothing:_) = []
whileJust (Just x:xs) = x : whileJust xs

findNthIndex :: Int -> (a -> Bool) -> [a] -> Maybe Int
findNthIndex i p | i < 1 = error "findNthIndex"
                 | otherwise = go 0 i
  where
    go _acc _ [] = Nothing
    go acc 1 (x:xs) | p x = Just acc
                    | otherwise = go (1 + acc) 1 xs
    go acc k (x:xs) | p x = go (1 + acc) (k-1) xs
                    | otherwise = go (1 + acc) k xs

splitLengths :: (a -> Bool) -> [a] -> [Int]
splitLengths splitHere = go 0
  where
    go _ [] = []
    go n (x:xs) | splitHere x = n : go 1 xs
                | otherwise = go (n+1) xs

splitInto :: [Int] -> [a] -> [[a]]
splitInto [] _ = []
splitInto (n:ns) xs = thisPart : splitInto ns nextParts
  where
    (thisPart, nextParts) = splitAt n xs

runTestM :: TestM () -> InitialState -> Result
runTestM test InitialState{..} =
    if null failures then succeeded
    else failed{ reason = unlines failures }
  where
    (_, (program, checks)) = execRWS (unTestM test) () 0

    failures = mapMaybe evalCheck checks
      where
        evalCheck :: Check -> Maybe String
        evalCheck (CheckTiming _step _cycles) = Nothing -- TODO
        evalCheck (CheckAssertion msg b) = do
            guard $ not $ evalObs b
            return msg

        evalObs :: Obs a -> a
        evalObs = runIdentity . runAp (Identity . evalObs') . unObs

        evalObs' :: Obs' a -> a
        evalObs' (Query step (Coyoneda k q)) = k $ evalQuery q !! (step + 1)

        evalQuery :: Query a -> [a]
        evalQuery (Reg A) = map head $ listS cpuA
        evalQuery (Reg X) = map head $ listS cpuX
        evalQuery (Reg Y) = map head $ listS cpuY
        evalQuery (Reg P) = map head $ listS cpuP
        evalQuery (Reg SP) = map head $ listS cpuSP
        evalQuery PC = map head $ listS cpuPC
        evalQuery (Mem addr) = map (! evalObs addr) rams

    cpuIn :: CPUIn CLK
    cpuIn = CPUIn{..}

    initialRAM' = initialRAM // zip [initialPC..] (concat program)

    rams :: [Matrix Addr Byte]
    rams = tail $ scanl (//) initialRAM' writes

    cpuMemR = rom cpuMemA (Just . (initialRAM' !))
    cpuIRQ = high
    cpuNMI = high
    cpuWait = low

    timings :: [Int]
    timings = take (2 + length program) . splitLengths (== Fetch1) . whileJust . fromS $ cpuState

    listS :: (Rep a) => Signal CLK a -> [[a]]
    listS = map catMaybes . splitInto timings . fromS

    writes :: [[(Addr, Byte)]]
    writes = map catMaybes $ listS pipe
      where
        pipe = packEnabled (isEnabled cpuMemW) $
               pack (cpuMemA, enabledVal cpuMemW)

    (CPUOut{..}, CPUDebug{..}) = cpu' cpuInit cpuIn
      where
        cpuInit = CPUInit{ initA = initialA
                         , initX = initialX
                         , initY = initialY
                         , initP = initialFlags
                         , initPC = Just initialPC
                         , initSP = initialSP
                         }
