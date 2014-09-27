{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.CPU where

import MOS6502.Opcodes
import MOS6502.Types
import MOS6502.Utils

import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Matrix
import qualified Data.Sized.Matrix as Matrix
import Data.Bits

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
    , cpuArgLo :: Signal clk Byte
    , cpuA :: Signal clk Byte
    , cpuX :: Signal clk Byte
    , cpuY :: Signal clk Byte
    , cpuP :: Signal clk Byte
    , cpuSP :: Signal clk Byte
    , cpuPC :: Signal clk Addr
    , cpuOp :: Signal clk Opcode
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
           | WaitWrite
           deriving (Show, Eq, Enum, Bounded)
type StateSize = X11

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

data Indirect s clk = ReadIndirect (Signal clk Byte -> RTL s clk ())
                    | WriteIndirect (RTL s clk (Signal clk Byte))
                    | ModifyDirect (Signal clk Byte -> RTL s clk (Signal clk Byte))
                    | ReadDirect (Signal clk Byte -> RTL s clk ())

data Microcode s clk = Opcode0 (RTL s clk ())
                     | Opcode1 (Signal clk Byte -> RTL s clk ())
                     | Opcode2 (Signal clk Addr -> RTL s clk ())
                     | OnZP (Signal clk Byte -> Signal clk Byte) (Indirect s clk)
                     | Jam

bitsToByte :: (Clock clk)
           => Matrix X8 (Signal clk Bool)
           -> Signal clk Byte
bitsToByte = bitwise . packMatrix

cpu :: (Clock clk) => CPUIn clk -> (CPUOut clk, CPUDebug clk)
cpu CPUIn{..} = runRTL $ do
    -- State
    s <- newReg Init
    rOp <- newReg BRK
    rArgLo <- newReg 0x00
    let argAddr = (unsigned cpuMemR `shiftL` 8) .|. unsigned (reg rArgLo)

    -- Registers
    rA <- newReg 0x00
    rX <- newReg 0x00
    rY <- newReg 0x00
    rSP <- newReg 0x00
    rPC <- newReg 0x0000 -- To be filled in by Init

    -- Flags
    fC <- newReg False
    fZ <- newReg False
    fI <- newReg False
    fD <- newReg False
    fB <- newReg False
    fV <- newReg False
    fN <- newReg False

    let flags = bitsToByte . Matrix.fromList $
                map reg [fC, fZ, fI, fD, fB, fV, fN] ++ [low]

    rNextA <- newReg 0x0000
    rNextW <- newReg Nothing

    let setA v = do
            rA := v
            fZ := v .==. 0
            fN := v .>=. 0x80

    let write addr val = do
            rNextA := addr
            rNextW := enabledS val
            s := pureS WaitWrite
        delay1 = return () -- TODO

    let op LDA_Imm = Opcode1 $ \imm -> do
            setA imm
        op LDA_ZP = OnZP id $ ReadDirect $ \v -> do
            setA v
        op LDA_Ind_X = OnZP (+ reg rX) $ ReadIndirect $ \v -> do
            setA v

        op STA_Abs = Opcode2 $ \addr -> do
            write addr (reg rA)
        op STA_ZP = Opcode1 $ \zp -> do
            write (unsigned zp) (reg rA)
        op STA_ZP_X = Opcode1 $ \zp -> do
            write (unsigned (zp + reg rX)) (reg rA)
        op STA_Abs_X = Opcode2 $ \addr -> do
            let addr' = addr + unsigned (reg rX)
            write addr' (reg rA)
            delay1
        op STA_Ind_X = OnZP (+ reg rX) $ WriteIndirect $ do
            return $ reg rA

        op LDX_Imm = Opcode1 $ \imm -> do
            rX := imm
        op LDX_ZP = OnZP id $ ReadDirect $ \v -> do
            rX := v

        op STX_ZP = Opcode1 $ \zp -> do
            write (unsigned zp) (reg rA)

        op INX = Opcode0 $ do
            rX := reg rX + 1
            delay1
        op INY = Opcode0 $ do
            rY := reg rY + 1
            delay1
        op TAX = Opcode0 $ do
            rX := reg rA
            delay1
        op TXA = Opcode0 $ do
            rA := reg rX
            delay1
        op TAY = Opcode0 $ do
            rY := reg rA
            delay1
        op TYA = Opcode0 $ do
            rA := reg rY
            delay1

        op CMP_Imm = Opcode1 $ \imm -> do
            fC := reg rA .>=. imm
            fZ := reg rA .==. imm
            fN := reg rA .>=. 0x80

        op INC_ZP = OnZP id $ ModifyDirect $ \v -> do
            let v' = v + 1
            fZ := v' .==. 0
            fN := v' .>=. 0x80
            return v'

        op DEC_ZP = OnZP id $ ModifyDirect $ \v -> do
            let v' = v - 1
            fZ := v' .==. 0
            fN := v' .>=. 0x80
            return v'

        op ASL_A = Opcode0 $ do
            rA := reg rA `shiftL` 1
            fC := reg rA .>=. 0x80

        op JMP_Abs = Opcode2 $ \addr -> do
            rPC := addr

        op BNE = Opcode1 $ \offset -> do
            WHEN (bitNot $ reg fZ) $ do
                rPC := reg rPC + 1 + unsigned offset
        op BEQ = Opcode1 $ \offset -> do
            WHEN (reg fZ) $ do
                rPC := reg rPC + 1 + unsigned offset

        op _ = Jam

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
              rPC := (reg rPC .&. 0xFF) .|. (unsigned cpuMemR `shiftL` 8)
              rNextA := var rPC
              s := pureS Fetch1
          Fetch1 -> do
              rOp := bitwise cpuMemR
              switch (var rOp) $ \k -> case op k of
                  Jam -> do
                      s := pureS Halt
                  Opcode0 act -> do
                      act
                  _ -> do
                      s := pureS Fetch2
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Fetch1
          Fetch2 -> do
              switch (reg rOp) $ \k -> case op k of
                  Opcode1 act -> do
                      act cpuMemR
                  OnZP toZP _ -> do
                      rNextA := unsigned $ toZP cpuMemR
                      s := pureS Indirect1
                  Opcode2 _ -> do
                      rArgLo := cpuMemR
                      s := pureS Fetch3
                  _ -> do
                      s := pureS Halt
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Fetch1
          Fetch3 -> do
              switch (reg rOp) $ \k -> case op k of
                  Opcode2 act -> do
                      act argAddr
                  _ -> do
                      s := pureS Halt
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Fetch1
          Indirect1 -> do
              switch (reg rOp) $ \k -> case op k of
                  OnZP _ (ReadDirect act) -> do
                      act cpuMemR
                      rNextA := reg rPC
                      s := pureS Fetch1
                  OnZP _ (ModifyDirect act) -> do
                      v <- act cpuMemR
                      rNextW := enabledS v
                      s := pureS WaitWrite
                  OnZP _ _ -> do
                      rArgLo := cpuMemR
                      rNextA := reg rNextA + 1
                      s := pureS Indirect2
                  _ -> return ()
              s := pureS Halt
          Indirect2 -> do
              switch (reg rOp) $ \k -> case op k of
                  OnZP _ (ReadIndirect _) -> do
                      rNextA := argAddr
                      s := pureS WaitRead
                  OnZP _ (WriteIndirect act) -> do
                      rNextA := argAddr
                      v <- act
                      rNextW := enabledS v
                      s := pureS WaitWrite
                  _ -> do
                      s := pureS Halt
          WaitRead -> do
              switch (reg rOp) $ \k -> case op k of
                  OnZP _ (ReadIndirect act) -> do
                      act cpuMemR
                      rNextA := reg rPC
                      s := pureS Fetch1
                  _ -> do
                      s := pureS Halt
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
        cpuOp = reg rOp
        cpuArgLo = reg rArgLo
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
