{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module MOS6502.ALU where

import MOS6502.Types

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

packALUOut :: ALUOut clk -> Signal clk (Enabled Bool, Enabled Bool)
packALUOut ALUOut{..} = pack (aluOutC, aluOutV)

unpackALUOut :: Signal clk (Enabled Bool, Enabled Bool) -> ALUOut clk
unpackALUOut (unpack -> (aluOutC, aluOutV)) = ALUOut{..}

data BinOp = ORA
           | AND
           | EOR
           | ADC
           | STA
           | LDA
           | CMP
           | SBC
           deriving (Show, Eq, Ord, Enum, Bounded)

instance BitRep BinOp where
    bitRep = bitRepEnum

$(repBitRep ''BinOp 3)

data UnOp = ASL
          | ROL
          | LSR
          | ROR
          | STX
          | LDX
          | DEC
          | INC
          deriving (Show, Eq, Ord, Enum, Bounded)

instance BitRep UnOp where
    bitRep = bitRepEnum

$(repBitRep ''UnOp 3)

binaryALU :: forall clk. (Clock clk)
          => ALUIn clk -> Signal clk Byte -> Signal clk Byte
          -> Signal clk BinOp
          -> (ALUOut clk, Signal clk Byte)
binaryALU flags arg1 arg2 op = (ALUOut{..}, result)
  where
    (result, aluOutC, aluOutV) = unpack $ ops .!. bitwise op

    ops :: Signal clk (Matrix (Unsigned (W BinOp)) (Byte, Enabled Bool, Enabled Bool))
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
        (c, v, z) = subCarry flags arg1 arg2
    staS = logicS (\x _ -> x)
    ldaS = logicS (\_ y -> y)
    cmpS = (arg1 - arg2, enabledS $ arg1 .>=. arg2, disabledS)

unaryALU :: forall clk. (Clock clk)
         => ALUIn clk -> Signal clk Byte
         -> Signal clk UnOp
         -> (ALUOut clk, Signal clk Byte)
unaryALU ALUIn{..} arg1 op = (ALUOut{..}, result)
  where
    (result, aluOutC) = unpack $ ops .!. bitwise op
    aluOutV = disabledS

    ops :: Signal clk (Matrix (Unsigned (W UnOp)) (Byte, Enabled Bool))
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
cmpALU arg1 arg2 = (ALUOut{..}, arg2 - arg1)
  where
    aluOutC = enabledS $ arg1 .<=. arg2
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
         => ALUIn clk
         -> Signal clk Byte
         -> Signal clk Byte
         -> (Signal clk Bool, Signal clk Bool, Signal clk Byte)
subCarry ALUIn{..} x y = (bitNot carry, overflow, z')
  where
    z = subExtend aluInC x y
    z' = signed z

    carry = testABit z 8
    -- http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    overflow = ((x `xor` z') .&. (y' `xor` z') .&. 0x80) ./=. 0x00
    y' = 0xff - y
