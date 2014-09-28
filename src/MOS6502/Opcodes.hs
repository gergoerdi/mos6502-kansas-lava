{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.Opcodes (Opcode(..)) where

import MOS6502.Types

import Language.KansasLava
import Data.Sized.Ix

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Maybe

data Opcode = LDA_Imm
            | LDA_ZP
            | LDA_Abs_X
            | LDA_Ind_X
            | LDA_Ind_Y
            | STA_ZP
            | STA_ZP_X
            | STA_Abs
            | STA_Abs_X
            | STA_Ind_X
            | STA_Ind_Y
            | INX
            | DEX
            | INY
            | DEY
            | TAX
            | TXA
            | TAY
            | TYA
            | INC_ZP
            | DEC_ZP
            | LDX_Imm
            | LDX_ZP
            | LDY_Abs_X
            | STX_ZP
            | JMP_Abs
            | BEQ
            | BNE
            | CLC
            | ADC_ZP
            | ASL_A
            | CMP_Imm
            | BRK
            deriving (Eq, Ord, Bounded, Enum)

opcodes :: Bimap Opcode Byte
opcodes = Bimap.fromList
          [ (LDA_Imm,   0xA9)
          , (LDA_ZP,    0xA5)
          , (LDA_Abs_X, 0xBD)
          , (LDA_Ind_X, 0xA1)
          , (LDA_Ind_Y, 0xB1)
          , (STA_ZP,    0x85)
          , (STA_ZP_X,  0x95)
          , (STA_Abs,   0x8D)
          , (STA_Abs_X, 0x9D)
          , (STA_Ind_X, 0x81)
          , (STA_Ind_Y, 0x91)
          , (INX,       0xE8)
          , (DEX,       0xCA)
          , (INY,       0xC8)
          , (DEY,       0x88)
          , (TAX,       0xAA)
          , (TXA,       0xBA)
          , (TAY,       0xA8)
          , (TYA,       0x98)
          , (INC_ZP,    0xE6)
          , (DEC_ZP,    0xC6)
          , (LDX_Imm,   0xA2)
          , (LDX_ZP,    0xA6)
          , (LDY_Abs_X, 0xBC)
          , (STX_ZP,    0x44)
          , (JMP_Abs,   0x4C)
          , (BEQ,       0xF0)
          , (BNE,       0xD0)
          , (CLC,       0x18)
          , (ADC_ZP,    0x75)
          , (ASL_A,     0x0A)
          , (CMP_Imm,   0xC9)
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