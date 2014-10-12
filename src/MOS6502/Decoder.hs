{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module MOS6502.Decoder (Addressing(..), Decoded(..), decode) where

import MOS6502.Types
import MOS6502.Utils
import MOS6502.ALU

import Language.Literals.Binary
import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Bits
import Data.Tuple (swap)

data Addressing clk = Addressing{ addrNone :: Signal clk Bool
                                , addrImm :: Signal clk Bool
                                , addrZP :: Signal clk Bool
                                , addrPreAddX :: Signal clk Bool
                                , addrPreAddY :: Signal clk Bool
                                , addrPostAddY :: Signal clk Bool
                                , addrIndirect :: Signal clk Bool
                                , addrDirect :: Signal clk Bool
                                , addrPop :: Signal clk Bool
                                }

data Decoded clk = Decoded{ dAddr :: Addressing clk
                          , dReadMem :: Signal clk Bool
                          , dReadA :: Signal clk Bool
                          , dReadX :: Signal clk Bool
                          , dReadY :: Signal clk Bool
                          , dUseBinALU :: Signal clk (Enabled BinOp)
                          , dUseUnALU :: Signal clk (Enabled UnOp)
                          , dUseCmpALU :: Signal clk Bool
                          , dUpdateFlags :: Signal clk Bool
                          , dWriteA :: Signal clk Bool
                          , dWriteX :: Signal clk Bool
                          , dWriteY :: Signal clk Bool
                          , dWriteMem :: Signal clk Bool
                          , dWriteFlags :: Signal clk Bool
                          , dBranch :: Signal clk (Enabled (U2, Bool))
                          , dSetFlag :: Signal clk (Enabled X8)
                          , dClearFlag :: Signal clk (Enabled X8)
                          , dJump :: Signal clk Bool
                          , dJSR :: Signal clk Bool
                          , dRTS :: Signal clk Bool
                          }

decode :: forall clk. (Clock clk) => Signal clk Byte -> Decoded clk
decode op = Decoded{..}
  where
    -- (opAAA, opBBBCC) = swap . unappendS $ op :: (Signal clk U3, Signal clk U5)
    -- (opBBB, opCC) = swap . unappendS $ opBBBCC :: (Signal clk U3, Signal clk U2)
    (opAAABBB, opCC) = swap . unappendS $ op :: (Signal clk U6, Signal clk U2)
    (opAAA, opBBB) = swap . unappendS $ opAAABBB :: (Signal clk U3, Signal clk U3)
    isBinOp = opCC .==. [b|01|]
    binOp = bitwise opAAA
    isUnOp = opCC .==. [b|10|] .&&.
             -- bitNot (unOp `elemS` [Un_Special_1, Un_Special_2]) .&&.
             bitNot (op .==. 0xEA) -- NOP
    unOp = bitwise opAAA
    isShift = isUnOp .&&. unOp `elemS` [ASL, ROL, LSR, ROR, LDX]
    isUnAcc = isShift .&&. opBBB .==. [b|010|]

    isSTY = opCC .==. [b|00|] .&&. opAAA .==. [b|100|]
    isLDY = opCC .==. [b|00|] .&&. opAAA .==. [b|101|]
    dJSR = op .==. 0x20
    dJump = op `elemS` [0x4C, 0x6C]
    dRTS = op .==. 0x60

    dAddr = Addressing{..}
      where
        addrNone = muxN [ (isBinOp, low)
                        , (isUnOp, isUnAcc)
                        , (isBranch .||. dJump, low)
                        , (opCC .==. [b|00|] .&&. opAAA ./=. [b|000|],
                             bitNot $ opBBB `elemS` [[b|000|], [b|001|], [b|011|], [b|101|], [b|111|]])
                        , (high, high)
                        ]
        addrImm = muxN [ (isBinOp, opBBB .==. [b|010|])
                       , (isUnOp, opBBB .==. [b|000|])
                       , (opCC .==. [b|00|], bitNot dJSR .&&. opBBB .==. [b|000|])
                       , (isBranch .||. dJump, high)
                       , (high, low)
                       ]
        addrZP = opBBB `elemS` [[b|001|], [b|101|]]

        useY = isUnOp .&&. opAAA `elemS` [[b|100|], [b|101|]] -- STX/LDX
        preAddX = muxN [ (isBinOp .||. isUnOp, opBBB `elemS` [[b|000|], [b|101|], [b|111|]])
                       , (opCC .==. [b|00|], opBBB `elemS` [[b|101|], [b|111|]])
                       , (high, low)
                       ]
        addrPreAddX = mux useY (preAddX, low)
        preAddY = isBinOp .&&. opBBB .==. [b|110|]
        addrPreAddY = mux useY (preAddY, preAddX)
        addrPostAddY = isBinOp .&&. opBBB .==. [b|100|]
        addrIndirect = muxN [ (isBinOp, opBBB `elemS` [[b|000|], [b|100|]])
                            , (high, op `elemS` [0x6C]) -- JMP
                            ]
        addrDirect = muxN [ (isBinOp, opBBB `elemS` [[b|011|], [b|110|], [b|111|]])
                          , (high,  dJSR .||. opBBB `elemS` [[b|011|], [b|111|]])
                          ]
        addrPop = op `elemS` [ 0x68, 0x28 ] -- PLA, PLP

    dUseBinALU = packEnabled isBinOp binOp
    dUseUnALU = muxN [ (isUnOp, enabledS unOp)
                     , (op `elemS` [0xE8, 0xC8], enabledS $ pureS INC) -- INX, INY
                     , (op `elemS` [0xCA, 0x88], enabledS $ pureS DEC) -- DEX, DEY
                     , (op .==. pureS 0xA8, enabledS $ pureS LDX) -- TAY
                     , (op .==. pureS 0x98, enabledS $ pureS STX) -- TYA
                     , (isSTY, enabledS $ pureS STX)
                     , (isLDY, enabledS $ pureS LDX)
                     , (high, disabledS)
                     ]
    dUseCmpALU = opCC .==. [b|00|] .&&.
                 opAAA `elemS` [[b|110|], [b|111|]] .&&.
                 opBBB `elemS` [[b|000|], [b|001|], [b|011|]]

    dReadA = muxN [ (isBinOp, binOp ./=. pureS LDA)
                  , (isUnOp, isUnAcc)
                  , (op .==. pureS 0xA8, high) -- TAY
                  , (high, low)
                  ]
    dReadX = muxN [ (isBinOp, low)
                  , (isUnOp, unOp .==. pureS STX)
                  , (op `elemS` [0xE8, 0xCA], high) -- INX, DEX
                  , (dUseCmpALU, opAAA .==. [b|111|])
                  , (high, low)
                  ]
    dReadY = muxN [ (isBinOp, low)
                  , (isUnOp, low)
                  , (op `elemS` [0xC8, 0x88], high) -- INY, DEY
                  , (op .==. pureS 0x98, high) -- TYA
                  , (dUseCmpALU, opAAA .==. [b|110|])
                  , (high, low)
                  ]
    dReadMem = muxN [ (isBinOp, bitNot dReadA)
                    , (isUnOp, bitNot $ dReadA .||. dReadX)
                    , (high, low)
                    ]

    dWriteA = muxN [ (isBinOp, binOp ./=. pureS STA)
                   , (isUnOp, isUnAcc)
                   , (op .==. pureS 0x98, high) -- TYA
                   , (op .==. pureS 0x68, high) -- PLA
                   , (high, low)
                   ]
    dWriteX = muxN [ (isBinOp, low)
                   , (isUnOp, unOp .==. pureS LDX)
                   , (op `elemS` [0xE8, 0xCA], high) -- INX, DEX
                   , (high, low)
                   ]
    dWriteY = muxN [ (isBinOp, low)
                   , (isUnOp, low)
                   , (op `elemS` [0xC8, 0x88, 0xA8], high) -- INY, DEY, TAY
                   , (isLDY, high)
                   , (high, low)
                   ]
    dWriteMem = muxN [ (isBinOp, binOp .==. pureS STA)
                     , (isUnOp, unOp ./=. pureS LDX .&&. bitNot isUnAcc)
                     , (high, low)
                     ]
    dWriteFlags = op .==. 0x28 -- PLP

    dUpdateFlags = muxN [ (isBinOp, binOp ./=. pureS STA)
                        , (isUnOp, unOp ./=. pureS STX)
                        , (op `elemS` [0xE8, 0xC8], high) -- INX, INY
                        , (op `elemS` [0xCA, 0x88], high) -- DEX, DEY
                        , (high, low)
                        ]

    isBranch = opCC .==. [b|00|] .&&. opBBB .==. [b|100|]
    dBranch = packEnabled isBranch $ pack (selector, targetValue)
      where
        selector = unsigned $ opAAA `shiftR` 1
        targetValue = opAAA `testABit` 0

    isChangeFlag = opBBB .==. [b|110|] .&&. opCC .==. [b|00|] .&&. opAAA ./=. [b|100|]
    setFlag = opAAA `elemS` [[b|001|], [b|011|], [b|111|]]
    clearFlag = bitNot setFlag
    flag = switchS opAAA [ ([b|000|], 0) -- C
                         , ([b|001|], 0) -- C
                         , ([b|010|], 2) -- I
                         , ([b|011|], 2) -- I
                         , ([b|101|], 6) -- V
                         , ([b|110|], 3) -- D
                         , ([b|111|], 3) -- D
                         ]
    dSetFlag = packEnabled (isChangeFlag .&&. setFlag) flag
    dClearFlag = packEnabled (isChangeFlag .&&. clearFlag) flag
