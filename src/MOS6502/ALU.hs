{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MOS6502.ALU where

import MOS6502.Types
import MOS6502.Utils

import Language.KansasLava
import Data.Sized.Signed
import Data.Sized.Unsigned
import Data.Sized.Matrix
import Data.Bits

data ALUIn clk = ALUIn { aluInC :: Signal clk Bool
                       , aluInD :: Signal clk Bool
                       }

data ALUOut clk = ALUOut{ aluOutC :: Signal clk (Enabled Bool)
                        , aluOutV :: Signal clk (Enabled Bool)
                        }

data BinAddr = Bin_Indirect_X
             | Bin_ZP
             | Bin_Imm
             | Bin_Absolute
             | Bin_Indirect_Y
             | Bin_ZP_X
             | Bin_Absolute_Y
             | Bin_Absolute_X
           deriving (Show, Eq, Enum, Bounded)
type BinAddrSize = X8

instance Rep BinAddr where
    type W BinAddr = X3 -- W BinAddrSize
    newtype X BinAddr = XBinAddr{ unXBinAddr :: Maybe BinAddr }

    unX = unXBinAddr
    optX = XBinAddr
    toRep s = toRep . optX $ s'
      where
        s' :: Maybe BinAddrSize
        s' = fmap (fromIntegral . fromEnum) $ unX s
    fromRep rep = optX $ fmap (toEnum . fromIntegral . toInteger) $ unX x
      where
        x :: X BinAddrSize
        x = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness BinAddrSize)

binIsLength2 :: (Clock clk) => Signal clk BinAddr -> Signal clk Bool
binIsLength2 addr = addr `elemS` [Bin_Imm, Bin_ZP, Bin_ZP_X]

binIsDirect :: (Clock clk) => Signal clk BinAddr -> Signal clk Bool
binIsDirect addr = addr `elemS` [Bin_Absolute, Bin_Absolute_X, Bin_Absolute_Y]

binIsIndirect :: (Clock clk) => Signal clk BinAddr -> Signal clk Bool
binIsIndirect addr = addr `elemS` [Bin_Indirect_X, Bin_Indirect_Y]

data UnAddr = Un_Imm
            | Un_ZP
            | Un_A
            | Un_Absolute
            | Un_Special_1
            | Un_ZP_X
            | Un_Special_2
            | Un_Absolute_X
            deriving (Show, Eq, Enum, Bounded)
type UnAddrSize = X8

instance Rep UnAddr where
    type W UnAddr = X3 -- W UnAddrSize
    newtype X UnAddr = XUnAddr{ unXUnAddr :: Maybe UnAddr }

    unX = unXUnAddr
    optX = XUnAddr
    toRep s = toRep . optX $ s'
      where
        s' :: Maybe UnAddrSize
        s' = fmap (fromIntegral . fromEnum) $ unX s
    fromRep rep = optX $ fmap (toEnum . fromIntegral . toInteger) $ unX x
      where
        x :: X UnAddrSize
        x = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness UnAddrSize)

unIsLength2 :: (Clock clk) => Signal clk UnAddr -> Signal clk Bool
unIsLength2 addr = addr `elemS` [Un_Imm, Un_ZP, Un_ZP_X]

unIsDirect :: (Clock clk) => Signal clk UnAddr -> Signal clk Bool
unIsDirect addr = addr `elemS` [Un_Absolute, Un_Absolute_X]

data BinOp = ORA
           | AND
           | EOR
           | ADC
           | STA
           | LDA
           | CMP
           | SBC
           deriving (Show, Eq, Enum, Bounded)
type BinOpSize = X8

instance Rep BinOp where
    type W BinOp = X3 -- W BinOpSize
    newtype X BinOp = XBinOp{ unXBinOp :: Maybe BinOp }

    unX = unXBinOp
    optX = XBinOp
    toRep s = toRep . optX $ s'
      where
        s' :: Maybe BinOpSize
        s' = fmap (fromIntegral . fromEnum) $ unX s
    fromRep rep = optX $ fmap (toEnum . fromIntegral . toInteger) $ unX x
      where
        x :: X BinOpSize
        x = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness BinOpSize)


binaryALU :: forall clk. (Clock clk)
          => Signal clk BinOp
          -> ALUIn clk -> Signal clk Byte -> Signal clk Byte
          -> (ALUOut clk, Signal clk Byte)
binaryALU op flags arg1 arg2 = (ALUOut{..}, result)
  where
    (result, aluOutC, aluOutV) = unpack $ ops .!. bitwise op

    ops :: Signal clk (Matrix BinOpSize (Byte, Enabled Bool, Enabled Bool))
    ops = pack $ matrix $ map pack $
          [ oraS
          , andS
          , eorS
          , adcS
          , staS
          , ldaS
          , cmpS
          , sbcS
          ]

    logicS f = (z, disabledS, disabledS)
      where
        z = f arg1 arg2

    oraS = logicS (.|.)
    andS = logicS (.&.)
    eorS = logicS xor
    adcS = (z, enabledS c, enabledS v)
      where
        (c, v, z) = addCarry flags arg1 arg2
    sbcS = (z, enabledS c, enabledS v)
      where
        (c, v, z) = sub
    staS = logicS (\x _ -> x)
    ldaS = logicS (\_ y -> y)
    cmpS = (arg1 - arg2, enabledS $ arg1 .>=. arg2, disabledS)

    sub = subCarry aluInC arg1 arg2

data UnOp = ASL
          | ROL
          | LSR
          | ROR
          | STX
          | LDX
          | DEC
          | INC
          deriving (Show, Eq, Enum, Bounded)
type UnOpSize = X8

instance Rep UnOp where
    type W UnOp = X3 -- W UnOpSize
    newtype X UnOp = XUnOp{ unXUnOp :: Maybe UnOp }

    unX = unXUnOp
    optX = XUnOp
    toRep s = toRep . optX $ s'
      where
        s' :: Maybe UnOpSize
        s' = fmap (fromIntegral . fromEnum) $ unX s
    fromRep rep = optX $ fmap (toEnum . fromIntegral . toInteger) $ unX x
      where
        x :: X UnOpSize
        x = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness UnOpSize)

unaryALU :: forall clk. (Clock clk)
         => Signal clk UnOp
         -> ALUIn clk -> Signal clk Byte
         -> (ALUOut clk, Signal clk Byte)
unaryALU op ALUIn{..} arg1 = (ALUOut{..}, result)
  where
    (result, aluOutC) = unpack $ ops .!. bitwise op
    aluOutV = disabledS

    ops :: Signal clk (Matrix UnOpSize (Byte, Enabled Bool))
    ops = pack $ matrix $ map pack $
          [ aslS
          , rolS
          , lsrS
          , rorS
          , stxS
          , ldxS
          , decS
          , incS
          ]

    aslS = (arg1 `shiftL` 1, enabledS $ arg1 `testABit` 7)
    rolS = (arg1 `shiftL` 1 .|. unsigned aluInC, enabledS $ arg1 `testABit` 7)
    lsrS = (arg1 `shiftR` 1, enabledS $ arg1 `testABit` 0)
    rorS = (arg1 `shiftR` 1 .|. unsigned aluInC `shiftL` 7, enabledS $ arg1 `testABit` 0)
    stxS = (arg1, disabledS)
    ldxS = (arg1, disabledS)
    decS = (arg1 - 1, disabledS)
    incS = (arg1 + 1, disabledS)

cmpALU :: forall clk. (Clock clk)
       => Signal clk Byte -> Signal clk Byte
       -> (ALUOut clk, Signal clk Byte)
cmpALU arg1 arg2 = (ALUOut{..}, arg1 - arg2)
  where
    aluOutC = enabledS $ arg1 .>=. arg2
    aluOutV = disabledS

addExtend :: (Clock clk)
          => Signal clk Bool
          -> Signal clk Byte
          -> Signal clk Byte
          -> Signal clk S9
addExtend c x y = unsigned x + unsigned y + unsigned c

addCarry :: (Clock clk)
         => ALUIn clk
         -> Signal clk Byte
         -> Signal clk Byte
         -> (Signal clk Bool, Signal clk Bool, Signal clk Byte)
addCarry ALUIn{..} x y = unpack $ mux aluInD (bin, dec)
  where
    bin = addCarryBin aluInC x y
    dec = addCarryDec aluInC x y

addCarryBin :: (Clock clk)
            => Signal clk Bool
            -> Signal clk Byte
            -> Signal clk Byte
            -> Signal clk (Bool, Bool, Byte)
addCarryBin c x y = pack (carry, overflow, z')
  where
    z = addExtend c x y
    z' = signed z

    carry = testABit z 8
    -- http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    overflow = ((x `xor` z') .&. (y `xor` z') .&. 0x80) ./=. 0x00

addCarryDec :: forall clk. (Clock clk)
            => Signal clk Bool
            -> Signal clk Byte
            -> Signal clk Byte
            -> Signal clk (Bool, Bool, Byte)
addCarryDec c x y = pack (carry, overflow, z)
  where
    loX, hiX, loY, hiY :: Signal clk U4
    (loX, hiX) = unappendS x
    (loY, hiY) = unappendS y

    loZ :: Signal clk U5
    loZ = unsigned c + unsigned loX + unsigned loY

    loC = loZ .>. 0x9

    loZ' :: Signal clk U4
    loZ' = mux loC (unsigned loZ, unsigned (loZ + 6))

    hiZ :: Signal clk U5
    hiZ = unsigned loC + unsigned hiX + unsigned hiY

    z = appendS loZ' (unsigned hiZ :: Signal clk U4)

    carry = testABit hiZ 4
    overflow = low -- TODO

subExtend :: (Clock clk)
          => Signal clk Bool
          -> Signal clk Byte
          -> Signal clk Byte
          -> Signal clk S9
subExtend c x y = unsigned x - unsigned y - unsigned (bitNot c)

subCarry :: forall clk. (Clock clk)
         => Signal clk Bool
         -> Signal clk Byte
         -> Signal clk Byte
         -> (Signal clk Bool, Signal clk Bool, Signal clk Byte)
subCarry c x y = (bitNot carry, overflow, z')
  where
    z = subExtend c x y
    z' = signed z

    carry = testABit z 8
    -- http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    overflow = ((x `xor` z') .&. (y' `xor` z') .&. 0x80) ./=. 0x00
    y' = 0xff - y
