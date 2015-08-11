{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module MOS6502.CPU where

import MOS6502.Types
import MOS6502.Utils
import MOS6502.ALU
import MOS6502.Decoder

import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix
import qualified Data.Sized.Matrix as Matrix
import Data.Bits
import Data.Default
import Control.Monad (zipWithM_)
import Control.Arrow (first)

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
    , cpuOp :: Signal clk Byte
    , cpuDecoded :: Decoded clk
    }

data State = Halt
           | Init
           | InitTest
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
           | WaitPushInt
           deriving (Show, Eq, Ord, Enum, Bounded)

instance BitRep State where
    bitRep = bitRepEnum

$(repBitRep ''State 4)

bitsToByte :: (Clock clk)
           => Matrix X8 (Signal clk Bool)
           -> Signal clk Byte
bitsToByte = bitwise . packMatrix

byteToBits :: (Clock clk)
           => Signal clk Byte
           -> Matrix X8 (Signal clk Bool)
byteToBits = unpackMatrix . bitwise

data CPUInit = CPUInit
    { initA :: Byte
    , initX :: Byte
    , initY :: Byte
    , initP :: Byte
    , initPC :: Maybe Addr
    , initSP :: Byte
    }

instance Default CPUInit where
    def = CPUInit{ initA = 0x00
                 , initX = 0x00
                 , initY = 0x00
                 , initP = 0x00
                 , initPC = Nothing
                 , initSP = 0xFF
                 }

cpu :: forall clk. (Clock clk) => CPUIn clk -> (CPUOut clk, CPUDebug clk)
cpu = cpu' def

cpu' :: forall clk. (Clock clk) => CPUInit -> CPUIn clk -> (CPUOut clk, CPUDebug clk)
cpu' CPUInit{..} CPUIn{..} = runRTL $ do

    -- State
    let (s0, pc0) = case initPC of
            Nothing -> (Init, 0x0000) -- PC to be filled in by Init
            Just pc -> (InitTest, pc)
    s <- newReg s0
    let ready = bitNot $ reg s `elemS` [Init, InitTest, Halt]
    rPC <- newReg pc0

    rArgBuf <- newReg 0x00
    let argByte = cpuMemR
    let argWord = reg rArgBuf `appendS` argByte

    -- Registers
    rA <- newReg initA
    rX <- newReg initX
    rY <- newReg initY
    rSP <- newReg initSP
    let popTarget = 0x0100 .|. unsigned (reg rSP + 1)
        pushTarget = 0x0100 .|. unsigned (reg rSP)

    -- Status flags
    rFlags@[fN, fV, _, _fB, fD, fI, fZ, fC] <- fmap reverse $ mapM (newReg . testBit initP) [0..7]

    let flags0 = bitsToByte . Matrix.fromList . map reg . reverse $ rFlags
        flags = flags0 .|. 0x20

        flagsBRK = flags .|. 0x10 -- B flag is always pushed as 1
        flagsIRQ = flags .&. (complement 0x10)

        writeFlags mtx = zipWithM_ (:=) (reverse rFlags) (Matrix.toList . byteToBits $ mtx)
        writeFlag i b = CASE [ IF (i .==. pureS (fromIntegral j)) $ rFlag := b
                             | (j, rFlag) <- zip [0..] (reverse rFlags)
                             ]

    let branchFlags :: Signal clk (Matrix (Unsigned (W BranchFlag)) Bool)
        branchFlags = pack . Matrix.fromList . map reg $ [fN, fV, fC, fZ]

    -- Hardware interrupts
    nmi <- newReg False
    irq <- newReg False

    WHEN ready $ do
        WHEN (fallingEdge cpuNMI) $ nmi := high
        WHEN (bitNot cpuIRQ .&&. bitNot (var fI)) $ irq := high
    newNMI <- newReg False
    newIRQ <- newReg False

    -- Decoder state
    rOp <- newReg 0x00
    let op = var rOp
        decoded@Decoded{..} = decode (var newNMI) (var newIRQ) op
        size1 = dAddrMode .==. pureS AddrNone
        size2 = dAddrMode `elemS` [AddrImm, AddrZP, AddrIndirect]
        _size3 = dAddrMode .==. pureS AddrDirect

    rNextA <- newReg 0x0000
    rNextW <- newReg Nothing

    let aluIn = ALUIn{ aluInC = reg fC, aluInD = reg fD }

    let sourceReg = switchS (enabledVal dSourceReg)
                    [ (RegX, reg rX)
                    , (RegY, reg rY)
                    , (RegA, reg rA)
                    , (RegSP, reg rSP)
                    ]

    let unArg = muxN [ (dReadMem, argByte)
                     , (dAddrMode .==. pureS AddrImm, argByte)
                     , (isEnabled dSourceReg, sourceReg)
                     ]
    let dALU = opALU dOp
        dUseBinALU = aluBinOp .=<<. dALU
        dUseUnALU = aluUnOp .=<<. dALU
        dUseCmpALU = packEnabled (isEnabled dALU .&&. enabledVal dALU .==. pureS ALUCmp) sourceReg
        dUseALU = isEnabled dUseBinALU .||. isEnabled dUseUnALU .||. isEnabled dUseCmpALU
        dBIT = isEnabled dALU .&&. enabledVal dALU .==. pureS ALUBIT

    let (ALUOut{..}, res) = first unpackALUOut . muxN2 $
                            [ aluOp dUseBinALU $ binaryALU aluIn (reg rA) argByte
                            , aluOp dUseUnALU $ unaryALU aluIn unArg
                            , aluOp dUseCmpALU $ cmpALU argByte
                            , (high, (pack (disabledS, disabledS), argByte))
                            ]
          where
            aluOp opS mkALU  = (isEnabled opS, first packALUOut . mkALU $ enabledVal opS)

    let writeTarget = do
            WHEN dUpdateFlags $ do
                WHEN dUseALU $ do
                    CASE [ match aluOutC (fC :=) ]
                    CASE [ match aluOutV (fV :=) ]
                CASE [ IF dBIT $ do
                            fZ := (reg rA .&. argByte) .==. 0
                            fV := argByte `testABit` 6
                            fN := argByte `testABit` 7
                     , OTHERWISE $ do
                            fZ := res .==. 0
                            fN := res `testABit` 7
                     ]

            CASE [ match dTargetReg $ flip switch $ \case
                        RegA -> rA := res
                        RegX -> rX := res
                        RegY -> rY := res
                        RegSP -> rSP := res
                 ]

    let (addrPreOffset, addrPostOffset) =
            muxN2 [ (dAddrOffset .==. pureS OffsetPreAddX, (reg rX, 0))
                  , (dAddrOffset .==. pureS OffsetPreAddY, (reg rY, 0))
                  , (dAddrOffset .==. pureS OffsetPostAddY, (0, reg rY))
                  , (high, (0, 0))
                  ]

    let addr1 = mux (dAddrMode `elemS` [AddrZP, AddrIndirect])
                      (argWord + unsigned addrPreOffset,
                       unsigned $ argByte + addrPreOffset)
        run1 = do
            caseEx [ match (opBranch dOp) $ \branch -> do
                          let (selector, target) = unpack branch
                          let branchFlag = branchFlags .!. bitwise selector
                              branchCond = branchFlag .==. target
                          WHEN branchCond $ do
                              rPC := reg rPC + signed argByte + 1
                          s := pureS Fetch1
                   , IF (dOp .==. pureS OpRTS) $ do
                          rSP := reg rSP + 2
                          rNextA := popTarget
                          s := pureS FetchVector1
                   , IF (dOp .==. pureS OpRTI) $ do
                          rSP := reg rSP + 1
                          rNextA := popTarget
                          s := pureS WaitRead
                   , IF (dOp .==. pureS (OpJumpCall Jump)) $ do
                          CASE [ IF dReadMem $ do
                                      rNextA := argWord
                                      s := pureS FetchVector1
                               , OTHERWISE $ do
                                      rPC := argWord
                                      s := pureS Fetch1
                               ]
                   , match (opPop dOp) $ \_arg -> do
                          rSP := reg rSP + 1
                          rNextA := popTarget
                          s := pureS WaitRead
                   , IF (dAddrMode `elemS` [AddrNone, AddrImm]) $ do
                          writeTarget
                          CASE [ match (opChangeFlag dOp) $ uncurry writeFlag . unpack ]
                          rNextA := var rPC
                          s := pureS Fetch1
                   , IF (dOp .==. pureS (OpJumpCall Call)) $ do
                          rArgBuf := unsigned (reg rPC)
                          rSP := reg rSP - 2
                          rNextA := pushTarget
                          rNextW := enabledS $ unsigned (reg rPC `shiftR` 8)
                          rPC := addr1
                          s := pureS WaitPushAddr
                   , IF (dAddrMode .==. pureS AddrIndirect) $ do
                          rNextA := addr1
                          s := pureS Indirect1
                   , IF dReadMem $ do
                          rNextA := addr1
                          s := pureS WaitRead
                   , IF dWriteMem $ do
                          rNextA := addr1
                          rNextW := enabledS res
                          s := pureS WaitWrite
                   ]

    let addr2 = argWord + unsigned addrPostOffset
    let run2 = do
            caseEx [ IF (dOp .==. pureS OpRTI) $ do
                          writeFlags argByte
                          rSP := reg rSP + 2
                          rNextA := popTarget
                          s := pureS FetchVector1
                   , IF (dAddrMode .==. pureS AddrIndirect .&&. reg s .==. pureS Indirect2) $ do
                          rNextA := addr2
                          CASE [ IF dWriteMem $ do
                                      rNextW := enabledS res
                                      s := pureS WaitWrite
                               , OTHERWISE $ do
                                      s := pureS WaitRead
                               ]
                   , OTHERWISE $ do
                          CASE [ match (opPop dOp) $ \arg -> do
                                     WHEN (arg .==. pureS StackArgP) $ writeFlags argByte
                               ]
                          writeTarget
                          CASE [ IF dWriteMem $ do
                                      rNextW := enabledS res
                                      s := pureS WaitWrite
                               , OTHERWISE $ do
                                      rNextA := reg rPC
                                      s := pureS Fetch1
                               ]
                   ]

    WHEN (bitNot cpuWait) $
      switch (reg s) $ \state -> case state of
          Init -> do
              fI := high
              rNextA := pureS resetVector
              s := pureS FetchVector1
          InitTest -> do
              rNextA := reg rPC
              s := pureS Fetch1
          FetchVector1 -> do
              rPC := unsigned cpuMemR
              let hi = reg rNextA .&. 0xFF00
                  lo = unsigned $ reg rNextA
              rNextA := hi .|. unsigned (lo + pureS (1 :: Byte))
              s := pureS FetchVector2
          FetchVector2 -> do
              let pc' = (reg rPC .&. 0xFF) .|. (unsigned cpuMemR `shiftL` 8)
              rPC := mux (dOp .==. pureS OpRTS) (pc', pc' + 1) -- BWAAAAH!
              rNextA := var rPC
              s := pureS Fetch1
          Fetch1 -> do
              rOp := cpuMemR
              caseEx [ match (opInterrupt dOp) $ \int -> do
                            rSP := reg rSP - 2
                            rNextA := pushTarget

                            let isBRK = int .==. pureS IntBRK
                                isNMI = int .==. pureS IntNMI
                            let pc = mux isBRK (reg rPC, reg rPC + 2)
                            rNextW := enabledS $ unsigned (pc `shiftR` 8)
                            rArgBuf := unsigned pc

                            rPC := mux isNMI (pureS irqVector, pureS nmiVector)
                            s := pureS WaitPushAddr
                            -- s := pureS Halt
                     , match (opPush dOp) $ \arg -> do
                            rSP := reg rSP - 1
                            rNextA := pushTarget
                            rNextW := enabledS $ mux (arg .==. pureS StackArgP) (reg rA, flagsBRK)
                            s := pureS WaitWrite
                     , match (opPop dOp) $ \_arg -> do
                            rSP := reg rSP + 1
                            rNextA := popTarget
                            s := pureS WaitRead
                     , IF size1 $ do
                            run1
                            s := pureS Fetch1
                     ]

              nmi := low
              irq := low
              newNMI := reg nmi
              newIRQ := reg irq .&&. bitNot (reg fI)

              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Fetch2
          Fetch2 -> do
              WHEN size2 $ do
                  run1
                  s := pureS Fetch1
              rPC := reg rPC + 1
              rArgBuf := cpuMemR
              rNextA := var rPC
              s := pureS Fetch3
          Fetch3 -> do
              run1
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Fetch1
          Indirect1 -> do
              rArgBuf := cpuMemR
              -- ZP wraps around, so if low part of address is stored
              -- in $FF, the high part should be looked for in $00
              rNextA := (reg rNextA + 1) .&. 0xFF
              s := pureS Indirect2
          Indirect2 -> do
              run2
              s := pureS Halt
          WaitRead -> do
              run2
              s := pureS Halt
          WaitPushAddr -> do
              rNextA := reg rNextA - 1
              rNextW := enabledS (reg rArgBuf)
              s := pureS WaitWrite
          WaitWrite -> do
              CASE [ match (opInterrupt dOp) $ \int -> do
                          rSP := reg rSP - 1
                          rNextA := pushTarget
                          -- set B on BRK only
                          rNextW := enabledS $ mux (int .==. pureS IntBRK) (flagsIRQ, flagsBRK)
                          s := pureS WaitPushInt
                   ]
              rNextA := reg rPC
              s := pureS Fetch1
          WaitPushInt -> do
              rNextA := reg rPC
              fI := high
              s := pureS FetchVector1
          Halt -> do
              s := pureS Halt

    rNextW := disabledS

    let cpuMemA = var rNextA
        cpuMemW = var rNextW

    -- Debug view
    let cpuState = reg s
        cpuOp = op
        cpuArgBuf = reg rArgBuf
        cpuDecoded = decoded
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
