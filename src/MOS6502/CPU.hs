{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MOS6502.CPU where

import MOS6502.Opcodes
import MOS6502.Types
import MOS6502.Utils
import MOS6502.ALU

import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix
import qualified Data.Sized.Matrix as Matrix
import Data.Bits
import Control.Monad ((<=<), void)
import Data.Tuple (swap)

data CPUIn clk = CPUIn
    { cpuMemR :: Signal clk Byte
    -- , cpuDBE :: Signal clk Bool
    -- , cpuRDY :: Signal clk Bool
    , cpuIRQ :: Signal clk ActiveLow
    , cpuNMI :: Signal clk ActiveLow
    -- , cpuSO :: Signal clk ActiveLow
    , cpuWait :: Signal clk Bool -- XXX KLUDGE
    }

data CPUOut clk = CPUOut
    { cpuMemA :: Signal clk Addr
    , cpuMemW :: Signal clk (Enabled Byte)
    -- , cpuSync :: Signal clk Bool
    }

data CPUDebug clk = CPUDebug
    { cpuState :: Signal clk State
    , cpuArgBuf :: Signal clk Byte
    , cpuA :: Signal clk Byte
    , cpuX :: Signal clk Byte
    , cpuY :: Signal clk Byte
    , cpuP :: Signal clk Byte
    , cpuSP :: Signal clk Byte
    , cpuPC :: Signal clk Addr
    , cpuOp :: Signal clk (U3, U3, U2)
    }

data State = Halt
           | Init
           | FetchVector1
           | FetchVector2
           | Fetch1
           | Fetch2
           | Fetch3
           | Indirect1
           | Indirect2
           | WaitRead
           | WaitPushAddr
           | WaitWrite
           deriving (Show, Eq, Enum, Bounded)
type StateSize = X12

instance Rep State where
    type W State = X4 -- W StateSize
    newtype X State = XState{ unXState :: Maybe State }

    unX = unXState
    optX = XState
    toRep s = toRep . optX $ s'
      where
        s' :: Maybe StateSize
        s' = fmap (fromIntegral . fromEnum) $ unX s
    fromRep rep = optX $ fmap (toEnum . fromIntegral . toInteger) $ unX x
      where
        x :: X StateSize
        x = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness StateSize)

bitsToByte :: (Clock clk)
           => Matrix X8 (Signal clk Bool)
           -> Signal clk Byte
bitsToByte = bitwise . packMatrix

byteToBits :: (Clock clk)
           => Signal clk Byte
           -> Matrix X8 (Signal clk Bool)
byteToBits = unpackMatrix . bitwise

cpu :: forall clk. (Clock clk) => CPUIn clk -> (CPUOut clk, CPUDebug clk)
cpu CPUIn{..} = runRTL $ do
    -- State
    s <- newReg Init
    rOp <- newReg 0x00
    let op = var rOp
        (opAAA, opBBBCC) = swap . unappendS $ op :: (Signal clk U3, Signal clk U5)
        (opBBB, opCC) = swap . unappendS $ opBBBCC :: (Signal clk U3, Signal clk U2)

    rArgBuf <- newReg 0x00
    let argByte = cpuMemR
    let argWord = reg rArgBuf `appendS` argByte

    -- Registers
    rA <- newReg 0x00
    rX <- newReg 0x00
    rY <- newReg 0x00
    rSP <- newReg 0xFF
    rPC <- newReg 0x0000 -- To be filled in by Init
    -- let popTarget = 0x0100 .|. unsigned (reg rSP + 1)
    --     pushTarget = 0x0100 .|. unsigned (reg rSP)

    -- Flags
    fC <- newReg False
    fZ <- newReg False
    fI <- newReg False
    fD <- newReg False
    fB <- newReg False
    fV <- newReg False
    fN <- newReg False

    let flags = bitsToByte . Matrix.fromList $
                [ reg fC
                , reg fZ
                , reg fI
                , reg fD
                , reg fB
                , high
                , reg fV
                , reg fN
                ]
        setFlags mtx = do
            let [c, z, i, d, b, _, v, n] = Matrix.toList . byteToBits $ mtx
            fC := c
            fZ := z
            fI := i
            fD := d
            fB := b
            fV := v
            fN := n

        branchFlags :: Signal clk (Matrix U2 Bool)
        branchFlags = pack . Matrix.fromList . map reg $ [fN, fV, fC, fZ]

    rNextA <- newReg 0x0000
    rNextW <- newReg Nothing

    let aluIn = ALUIn{ aluInC = reg fC, aluInD = reg fD }
        commitALUFlags ALUOut{..} w = do
            CASE [ match aluOutC (fC :=) ]
            CASE [ match aluOutV (fV :=) ]
            fZ := w .==. 0
            fN := w `testABit` 7

    let binOp = bitwise opAAA
        binAddr = bitwise opBBB
    commitBinALU <- do
        let (aluOut, a') = binaryALU binOp aluIn (reg rA) argByte
        return $ do
            commitALUFlags aluOut a'
            rA := a'

    let unOp = muxN [ (op .==. 0xE8, pureS INC) -- INX
                    , (op .==. 0xC8, pureS INC) -- INY
                    , (op .==. 0xCA, pureS DEC) -- DEX
                    , (op .==. 0x88, pureS DEC) -- DEY
                    , (high, bitwise opAAA) ]
        unAddr = bitwise opBBB
        unOffset = mux (unOp `elemS` [STX, LDX]) (reg rX, reg rY)
        unArg = muxN [ (unAddr .==. pureS Un_A,
                          muxN [ (op .==. 0xE8, reg rX) -- INX
                               , (op .==. 0xC8, reg rY) -- INY
                               , (op .==. 0xCA, reg rX) -- DEX
                               , (op .==. 0x88, reg rY) -- DEY
                               , (op .==. 0xa8, reg rA) -- TAY
                               , (op .==. 0x98, reg rY) -- TYA
                               ])
                     , (opBBB .==. 0x6, reg rY) -- TYA
                     , (unOp .==. pureS STX, reg rX)
                     , (high, argByte)
                     ]
    let (unALUOut, unRes) = unaryALU unOp aluIn unArg
    let commitUnALU = commitALUFlags unALUOut unRes
    let commitUnResult = do
            fZ := unRes .==. 0
            fN := unRes `testABit` 7

    let branchFlag = branchFlags .!. (unsigned $ opAAA `shiftR` 1)
        branchCond = branchFlag .==. (opAAA `testABit` 0)

    WHEN (bitNot cpuWait) $
      switch (reg s) $ \state -> case state of
          Init -> do
              rNextA := pureS resetVector
              s := pureS FetchVector1
          FetchVector1 -> do
              rPC := unsigned cpuMemR
              rNextA := reg rNextA + 1
              s := pureS FetchVector2
          FetchVector2 -> do
              let pc' = (reg rPC .&. 0xFF) .|. (unsigned cpuMemR `shiftL` 8)
                  isRTS = reg rOp .==. pureS 0x60
              rPC := mux isRTS (pc', pc' + 1) -- BWAAAAH!
              rNextA := var rPC
              s := pureS Fetch1
          Fetch1 -> do
              rOp := cpuMemR
              switch opCC $ \cc -> case cc of
                  0x0 -> CASE [ IF (opBBB .==. 0x4) $ do -- Branch
                                     return ()
                              , OTHERWISE $ do
                                     CASE [ IF (op `elemS` [0xE8, 0xCA]) $ do -- INX, DEX
                                                 commitUnResult
                                                 rX := unRes
                                                 s := pureS Fetch1
                                          , IF (op `elemS` [0xC8, 0x88, 0xA8]) $ do -- INY, DEY, TAY
                                                 commitUnResult
                                                 rY := unRes
                                                 s := pureS Fetch1
                                          , IF (op .==. 0x98) $ do -- TYA
                                                 commitUnResult
                                                 rA := unRes
                                                 s := pureS Fetch1
                                          ]
                              ]
                  0x1 -> return () -- These are all 2- or 3-length instructions
                  0x2 -> do
                      CASE [ IF (unAddr .==. pureS Un_A) $ do
                                  CASE [ IF (unOp .==. pureS INC) $ do -- NOP
                                              return ()
                                       , IF (unOp .==. pureS LDX) $ do
                                              commitUnALU
                                              rX := unRes
                                       , OTHERWISE $ do
                                              commitUnALU
                                              rA := unRes
                                       ]
                           -- , IF (unAddr .==. Un_Special_2)
                           ]
                      s := pureS Fetch1
                  _ -> s := pureS Halt
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Fetch2
          Fetch2 -> do
              switch opCC $ \cc -> case cc of
                  0x0 -> CASE [ IF (opBBB .==. 0x4) $ do
                                     WHEN branchCond $
                                       rPC := reg rPC + signed argByte
                                     s := pureS Fetch1
                              ]
                  0x1 -> WHEN (binIsLength2 binAddr) $ do
                      CASE [ IF (binAddr .==. pureS Bin_Imm) $ do
                                  commitBinALU
                                  s := pureS Fetch1
                           , OTHERWISE $ do
                                  rNextA := unsigned $ switchS binAddr
                                    [ (Bin_ZP, argByte)
                                    , (Bin_ZP_X, argByte + reg rX)
                                    ]
                                  CASE [ IF (binOp .==. pureS STA) $ do
                                              rNextW := enabledS (reg rA)
                                              s := pureS WaitWrite
                                       , OTHERWISE $ do
                                              s := pureS WaitRead
                                       ]
                           ]
                  0x2 -> WHEN (unIsLength2 unAddr) $ do
                      CASE [ IF (unAddr .==. pureS Un_Imm) $ do
                                  commitUnALU
                                  rX := unRes
                                  s := pureS Fetch1
                           , OTHERWISE $ do
                                  rNextA := unsigned $ switchS unAddr
                                    [ (Un_ZP, argByte)
                                    , (Un_ZP_X, argByte + unOffset)
                                    ]
                                  CASE [ IF (unOp .==. pureS STX) $ do
                                              rNextW := enabledS unRes
                                              s := pureS WaitWrite
                                       , OTHERWISE $ do
                                              s := pureS WaitRead
                                       ]
                           ]
                  _ -> s := pureS Halt
              rPC := reg rPC + 1
              rArgBuf := cpuMemR
              rNextA := var rPC
              s := pureS Fetch3
          Fetch3 -> do
              switch opCC $ \cc -> case cc of
                  0x1 -> do
                      rNextA := switchS binAddr
                        [ (Bin_Absolute, argWord)
                        , (Bin_Absolute_X, argWord + unsigned (reg rX))
                        , (Bin_Absolute_Y, argWord + unsigned (reg rY))
                        , (Bin_Indirect_X, argWord + unsigned (reg rX))
                        , (Bin_Indirect_Y, argWord)
                        ]
                      CASE [ IF (binIsIndirect binAddr) $ do
                                  s := pureS Indirect1
                           , IF (binOp .==. pureS STA) $ do
                                  rNextW := enabledS (reg rA)
                                  s := pureS WaitWrite
                           , OTHERWISE $ do
                                  s := pureS WaitRead
                           ]
                  0x2 -> do
                      rNextA := switchS unAddr
                        [ (Un_Absolute, argWord)
                        , (Un_Absolute_X, argWord + unsigned unOffset)
                        ]
                      CASE [ IF (unOp .==. pureS STX) $ do
                                  rNextW := enabledS unRes
                                  s := pureS WaitWrite
                           , OTHERWISE $ do
                                  s := pureS WaitRead
                           ]
                  _ -> do
                      s := pureS Halt
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Fetch1
          Indirect1 -> switch opCC $ \cc -> case cc of
              0x1 -> do
                  rArgBuf := cpuMemR
                  rNextA := reg rNextA + 1
                  s := pureS Indirect2
              _ -> do
                  s := pureS Halt
          Indirect2 -> switch opCC $ \cc -> case cc of
              0x1 -> do
                  rNextA := switchS binAddr
                    [ (Bin_Indirect_X, argWord)
                    , (Bin_Indirect_Y, argWord + unsigned (reg rY))
                    ]
                  CASE [ IF (binOp .==. pureS STA) $ do
                              rNextW := enabledS (reg rA)
                              s := pureS WaitWrite
                       , OTHERWISE $ do
                              s := pureS WaitRead
                       ]
              _ -> do
                  s := pureS Halt
          WaitRead -> do
              switch opCC $ \cc -> case cc of
                  0x1 -> do
                      commitBinALU
                  0x2 -> do
                      commitUnALU
                      CASE [ IF (unOp .==. pureS LDX) $ do
                                  rX := argByte
                           , OTHERWISE $ do
                                  rNextW := enabledS argByte
                                  s := pureS WaitWrite
                           ]
                  _ -> do
                      s := pureS Halt
              rNextA := reg rPC
              s := pureS Fetch1
{-
          WaitPushAddr -> do
              rNextA := reg rNextA - 1
              rNextW := enabledS (reg rArgBuf)
              s := pureS WaitWrite
-}
          WaitWrite -> do
              rNextW := disabledS
              rNextA := reg rPC
              s := pureS Fetch1
          _ -> do
              s := pureS Halt

    let cpuMemA = var rNextA
        cpuMemW = var rNextW

    -- Debug view
    let cpuState = reg s
        cpuOp = pack (opAAA, opBBB, opCC)
        cpuArgBuf = reg rArgBuf
    let cpuA = reg rA
        cpuX = reg rX
        cpuY = reg rY
        cpuSP = reg rSP
        cpuP = flags
        cpuPC = reg rPC

    return (CPUOut{..}, CPUDebug{..})

resetVector :: Addr
resetVector = 0xFFFC

nmiVector :: Addr
nmiVector = 0xFFFA

irqVector :: Addr
irqVector = 0xFFFE
