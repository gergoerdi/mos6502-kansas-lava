{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.Opcodes (Opcode(..), AOperand(..)) where

import MOS6502.Types

import Language.KansasLava
import Data.Sized.Ix

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Maybe

data Opcode = LDA_Imm
            | LDA_ZP
            | LDA_ZP_X
            | LDA_Abs
            | LDA_Abs_X
            | LDA_Abs_Y
            | LDA_Ind_X
            | LDA_Ind_Y

            | LDX_Imm
            | LDX_ZP
            | LDX_ZP_Y
            | LDX_Abs
            | LDX_Abs_Y

            | LDY_Imm
            | LDY_ZP
            | LDY_ZP_X
            | LDY_Abs
            | LDY_Abs_X

            | STA_ZP
            | STA_ZP_X
            | STA_Abs
            | STA_Abs_X
            | STA_Abs_Y
            | STA_Ind_X
            | STA_Ind_Y

            | STX_ZP
            | STX_ZP_Y
            | STX_Abs

            | STY_ZP
            | STY_ZP_X
            | STY_Abs

            | INX
            | DEX
            | INY
            | DEY

            | TAX
            | TXA
            | TAY
            | TYA

            | TSX
            | TXS

            | INC_ZP
            | INC_ZP_X
            | INC_Abs
            | INC_Abs_X

            | DEC_ZP
            | DEC_ZP_X
            | DEC_Abs
            | DEC_Abs_X

            | AND_Imm
            | AND_ZP
            | AND_ZP_X
            | AND_Abs
            | AND_Abs_X
            | AND_Abs_Y
            | AND_Ind_X
            | AND_Ind_Y

            | ORA_Imm
            | ORA_ZP
            | ORA_ZP_X
            | ORA_Abs
            | ORA_Abs_X
            | ORA_Abs_Y
            | ORA_Ind_X
            | ORA_Ind_Y

            | EOR_Imm
            | EOR_ZP
            | EOR_ZP_X
            | EOR_Abs
            | EOR_Abs_X
            | EOR_Abs_Y
            | EOR_Ind_X
            | EOR_Ind_Y

            | ADC_Imm
            | ADC_ZP
            | ADC_ZP_X
            | ADC_Abs
            | ADC_Abs_X
            | ADC_Abs_Y
            | ADC_Ind_X
            | ADC_Ind_Y

            | SBC_Imm
            | SBC_ZP
            | SBC_ZP_X
            | SBC_Abs
            | SBC_Abs_X
            | SBC_Abs_Y
            | SBC_Ind_X
            | SBC_Ind_Y

            | CLC
            | SEC
            | CLI
            | SEI
            | CLV
            | CLD
            | SED

            | CMP_Imm
            | CMP_ZP
            | CMP_ZP_X
            | CMP_Abs
            | CMP_Abs_X
            | CMP_Abs_Y
            | CMP_Ind_X
            | CMP_Ind_Y

            | CPX_Imm
            | CPX_ZP
            | CPX_Abs

            | CPY_Imm
            | CPY_ZP
            | CPY_Abs

            | JMP_Abs
            | JMP_Ind
            | JSR
            | RTS

            | BEQ
            | BNE
            | BCS
            | BCC
            | BPL
            | BMI
            | BVS
            | BVC

            | PHA
            | PLA
            | ASL_A
            | LSR_A
            | BRK
            deriving (Eq, Ord, Bounded, Enum)

data AOperand = Imm
              | ZP
              | ZP_X
              | Abs
              | Abs_X
              | Abs_Y
              | Ind_X
              | Ind_Y
              deriving (Eq, Ord, Bounded, Enum)

opcodes :: Bimap Opcode Byte
opcodes = Bimap.fromList
          [ (LDA_Imm,   0xA9)
          , (LDA_ZP,    0xA5)
          , (LDA_ZP_X,  0xB5)
          , (LDA_Abs,   0xAD)
          , (LDA_Abs_X, 0xBD)
          , (LDA_Abs_Y, 0xB9)
          , (LDA_Ind_X, 0xA1)
          , (LDA_Ind_Y, 0xB1)

          , (LDX_Imm,   0xA2)
          , (LDX_ZP,    0xA6)
          , (LDX_ZP_Y,  0xB6)
          , (LDX_Abs,   0xAE)
          , (LDX_Abs_Y, 0xBE)

          , (LDY_Imm,   0xA0)
          , (LDY_ZP,    0xA4)
          , (LDY_ZP_X,  0xB4)
          , (LDY_Abs,   0xAC)
          , (LDY_Abs_X, 0xBC)

          , (STA_ZP,    0x85)
          , (STA_ZP_X,  0x95)
          , (STA_Abs,   0x8D)
          , (STA_Abs_X, 0x9D)
          , (STA_Abs_X, 0x99)
          , (STA_Ind_X, 0x81)
          , (STA_Ind_Y, 0x91)

          , (STX_ZP,    0x86)
          , (STX_ZP_Y,  0x96)
          , (STX_Abs,   0x8E)

          , (STY_ZP,    0x84)
          , (STY_ZP_X,  0x94)
          , (STY_Abs,   0x8C)

          , (INX,       0xE8)
          , (DEX,       0xCA)
          , (INY,       0xC8)
          , (DEY,       0x88)

          , (TAX,       0xAA)
          , (TXA,       0x8A)
          , (TAY,       0xA8)
          , (TYA,       0x98)
          , (TSX,       0xBA)
          , (TXS,       0x9A)

          , (INC_ZP,    0xE6)
          , (INC_ZP_X,  0xF6)
          , (INC_Abs,   0xEE)
          , (INC_Abs_X, 0xFE)

          , (DEC_ZP,    0xC6)
          , (DEC_ZP_X,  0xD6)
          , (DEC_Abs,   0xCE)
          , (DEC_Abs_X, 0xDE)

          , (AND_Imm,   0x29)
          , (AND_ZP,    0x25)
          , (AND_ZP_X,  0x35)
          , (AND_Abs,   0x2D)
          , (AND_Abs_X, 0x3D)
          , (AND_Abs_Y, 0x39)
          , (AND_Ind_X, 0x21)
          , (AND_Ind_Y, 0x31)

          , (ORA_Imm,   0x09)
          , (ORA_ZP,    0x05)
          , (ORA_ZP_X,  0x15)
          , (ORA_Abs,   0x0D)
          , (ORA_Abs_X, 0x1D)
          , (ORA_Abs_Y, 0x19)
          , (ORA_Ind_X, 0x01)
          , (ORA_Ind_Y, 0x11)

          , (EOR_Imm,   0x49)
          , (EOR_ZP,    0x45)
          , (EOR_ZP_X,  0x55)
          , (EOR_Abs,   0x4D)
          , (EOR_Abs_X, 0x5D)
          , (EOR_Abs_Y, 0x59)
          , (EOR_Ind_X, 0x41)
          , (EOR_Ind_Y, 0x51)

          , (ADC_Imm,   0x69)
          , (ADC_ZP,    0x65)
          , (ADC_ZP_X,  0x75)
          , (ADC_Abs,   0x6D)
          , (ADC_Abs_X, 0x7D)
          , (ADC_Abs_Y, 0x79)
          , (ADC_Ind_X, 0x61)
          , (ADC_Ind_Y, 0x71)

          , (SBC_Imm,   0xE9)
          , (SBC_ZP,    0xE5)
          , (SBC_ZP_X,  0xF5)
          , (SBC_Abs,   0xED)
          , (SBC_Abs_X, 0xFD)
          , (SBC_Abs_Y, 0xF9)
          , (SBC_Ind_X, 0xE1)
          , (SBC_Ind_Y, 0xF1)

          , (CLC,       0x18)
          , (SEC,       0x38)
          , (CLI,       0x58)
          , (SEI,       0x78)
          , (CLV,       0xB8)
          , (CLD,       0xD8)
          , (SED,       0xF8)

          , (CMP_Imm,   0xC9)
          , (CMP_ZP,    0xC5)
          , (CMP_ZP_X,  0xD5)
          , (CMP_Abs,   0xCD)
          , (CMP_Abs_X, 0xDD)
          , (CMP_Abs_Y, 0xD9)
          , (CMP_Ind_X, 0xC1)
          , (CMP_Ind_Y, 0xD1)

          , (CPX_Imm,   0xE0)
          , (CPX_ZP,    0xE4)
          , (CPX_Abs,   0xEC)

          , (CPY_Imm,   0xC0)
          , (CPY_ZP,    0xC4)
          , (CPY_Abs,   0xCC)

          , (JMP_Abs,   0x4C)
          , (JMP_Ind,   0x6C)
          , (JSR,       0x20)
          , (RTS,       0x60)

          , (BEQ,       0xF0)
          , (BNE,       0xD0)
          , (BCS,       0xB0)
          , (BCC,       0x90)
          , (BPL,       0x10)
          , (BMI,       0x30)
          , (BVC,       0x50)
          , (BVS,       0x70)

          , (PHA,       0x48)
          , (PLA,       0x68)
          , (ASL_A,     0x0A)
          , (LSR_A,     0x4A)
          , (BRK,       0x00)
          ]

instance Rep Opcode where
    type W Opcode = X8
    newtype X Opcode = XOpcode{ unXOpcode :: Maybe Opcode }

    unX = unXOpcode
    optX = XOpcode

    toRep = toRep . optX . fmap encode . unX
      where
        encode :: Opcode -> Byte
        encode op = fromMaybe 0x00 $ Bimap.lookup op opcodes

    fromRep = optX . fmap decode . unX . sizedFromRepToIntegral
      where
        decode :: Byte -> Opcode
        decode code = fromMaybe BRK $ Bimap.lookupR code opcodes

    repType _ = repType (Witness :: Witness Byte)
