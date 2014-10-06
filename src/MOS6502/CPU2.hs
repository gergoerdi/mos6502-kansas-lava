{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.CPU2 where

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

data Indexing = Unindexed
              | ByX
              | ByY

data AddrMode = AddrZP Indexing
              | AddrAbsolute Indexing
              | AddrIndirect Indexing Indexing

data ALUArg = ArgImm
            | ArgAddr AddrMode

data Register = RegA
              | RegX
              | RegY

data ALUDest = DestReg Register
             | DestAddr AddrMode

data PushDest = PushA
              | PushP

data BranchFlag = BranchN
                | BranchC
                | BranchV
                | BranchZ

data DecodedInstr = BinALU BinOp ALUArg ALUDest
                  | UnALU UnOp ALUDest
                  | Store Register ALUDest
                  | Compare Register ALUArg
                  | Branch BranchFlag Bool
                  | Push PushDest
                  | Pop PushDest
                  | JumpAbs
                  | JumpVector
                  | Interrupt Bool
                  | Jam

decode :: Opcode -> DecodedInstr
decode LDA_Imm = BinALU Copy ArgImm (DestReg RegA)
decode LDA_ZP = BinALU Copy (ArgAddr $ AddrZP Unindexed) (DestReg RegA)
decode LDA_ZP_X = BinALU Copy (ArgAddr $ AddrZP ByX) (DestReg RegA)
-- decode LDA_Abs = undefined
-- decode LDA_Abs_X = undefined
-- decode LDA_Abs_Y = undefined
-- decode LDA_Ind_X = undefined
-- decode LDA_Ind_Y = undefined
decode LDX_Imm = BinALU Copy ArgImm (DestReg RegX)
-- decode LDX_ZP = undefined
-- decode LDX_ZP_Y = undefined
-- decode LDX_Abs = undefined
-- decode LDX_Abs_Y = undefined
decode LDY_Imm = BinALU Copy ArgImm (DestReg RegY)
-- decode LDY_ZP = undefined
-- decode LDY_ZP_X = undefined
-- decode LDY_Abs = undefined
-- decode LDY_Abs_X = undefined
decode STA_ZP = Store RegA (DestAddr $ AddrZP Unindexed)
-- decode STA_ZP_X
-- decode STA_Abs
-- decode STA_Abs_X
-- decode STA_Abs_Y
-- decode STA_Ind_X
-- decode STA_Ind_Y
-- decode STX_ZP
-- decode STX_ZP_Y
-- decode STX_Abs
-- decode STY_ZP
-- decode STY_ZP_X
-- decode STY_Abs
decode INC_ZP = UnALU Inc (DestAddr $ AddrZP Unindexed)
decode INX = UnALU Inc (DestReg RegX)
decode DEX = UnALU Dec (DestReg RegX)
decode INY = UnALU Inc (DestReg RegY)
decode DEY = UnALU Dec (DestReg RegY)
decode TAX = Store RegA (DestReg RegX)
decode TAY = Store RegA (DestReg RegY)
decode TXA = Store RegX (DestReg RegA)
decode TYA = Store RegY (DestReg RegA)

decode AND_Imm = BinALU And ArgImm (DestReg RegA)
-- decode AND_ZP
-- decode AND_ZP_X
-- decode AND_Abs
-- decode AND_Abs_X
-- decode AND_Abs_Y
-- decode AND_Ind_X
-- decode AND_Ind_Y
decode CMP_Imm = Compare RegA ArgImm
decode CPX_Imm = Compare RegX ArgImm
decode ASL_A = UnALU ShiftL (DestReg RegA)

decode JMP_Abs = JumpAbs
decode JMP_Ind = JumpVector

decode _ = Jam

    --     op TSX = Op 2 $ Opcode0 $ setX $ reg rSP
    --     op TXS = Op 2 $ Opcode0 $ rSP := reg rX

    --     op ORA_Imm = aluA (.|.) Imm
    --     op EOR_Imm = aluA xor Imm
    --     op ADC_Imm = aluA' adc Imm
    --     op SBC_Imm = aluA' sbc Imm

    --     op CLC = Op 2 $ Opcode0 $ fC := low
    --     op SEC = Op 2 $ Opcode0 $ fC := high
    --     op CLI = Op 2 $ Opcode0 $ fI := low
    --     op SEI = Op 2 $ Opcode0 $ fI := high
    --     op CLV = Op 2 $ Opcode0 $ fV := low
    --     op CLD = Op 2 $ Opcode0 $ fD := low
    --     op SED = Op 2 $ Opcode0 $ fD := high

    --     op BIT_ZP = Op 3 $ OnZP id $ ReadDirect . const $ \v -> do
    --         fZ := v .&. reg rA .==. 0
    --         fV := v `testABit` 6
    --         fN := v `testABit` 7
    --     op BIT_Abs = Op 4 $ OnAddr id $ ReadDirect . const $ \v -> do
    --         fZ := v .&. reg rA .==. 0
    --         fV := v `testABit` 6
    --         fN := v `testABit` 7

    --     op JMP_Abs = Op 3 $ Opcode2 $ \addr -> do
    --         rPC := addr
    --     op JMP_Ind = Op 5 $ Opcode2 $ \addr -> do
    --         rNextA := addr
    --         s := pureS FetchVector1

    --     op JSR = Op 6 $ Opcode2 $ \addr -> do
    --         rNextA := pushTarget
    --         rNextW := enabledS (unsigned $ reg rPC `shiftR` 8)
    --         rArgBuf := unsigned (reg rPC)
    --         rSP := reg rSP - 2
    --         s := pureS WaitPushAddr
    --         rPC := addr

    --     op RTS = Op 6 $ Opcode0 $ do
    --         rSP := reg rSP + 2
    --         rNextA := popTarget
    --         s := pureS FetchVector1

    --     op PHA = Op 3 $ Opcode0 $ do
    --         write pushTarget (reg rA)
    --         rSP := reg rSP - 1
    --     op PLA = Op 4 $ PopByte setA

    --     op PHP = Op 3 $ Opcode0 $ do
    --         write pushTarget flags
    --         rSP := reg rSP - 1
    --     op PLP = Op 4 $ PopByte setFlags

    --     op BNE = branch (bitNot $ reg fZ)
    --     op BEQ = branch (reg fZ)
    --     op BCC = branch (bitNot $ reg fC)
    --     op BCS = branch (reg fC)
    --     op BVC = branch (bitNot $ reg fV)
    --     op BVS = branch (reg fV)
    --     op BPL = branch (bitNot $ reg fN)
    --     op BMI = branch (reg fN)

    --     op NOP = Op 2 $ Opcode0 $ return ()

    --     op _ = Jam

{-
data Indirect s clk = ReadIndirect (Signal clk Addr -> Signal clk Addr) (Signal clk Byte -> RTL s clk ())
                    | WriteIndirect (Signal clk Addr -> Signal clk Addr) (RTL s clk (Signal clk Byte))
                    | ReadDirect (Signal clk Addr -> Signal clk Byte -> RTL s clk ())

data Microcode s clk = Opcode0 (RTL s clk ())
                     | Opcode1 (Signal clk Byte -> RTL s clk ())
                     | Opcode2 (Signal clk Addr -> RTL s clk ())
                     | OnZP (Signal clk Byte -> Signal clk Byte) (Indirect s clk)
                     | OnAddr (Signal clk Addr -> Signal clk Addr) (Indirect s clk)
                     | PopByte (Signal clk Byte -> RTL s clk ())

data Op s clk = Op Int (Microcode s clk)
              | Jam

bitsToByte :: (Clock clk)
           => Matrix X8 (Signal clk Bool)
           -> Signal clk Byte
bitsToByte = bitwise . packMatrix

byteToBits :: (Clock clk)
           => Signal clk Byte
           -> Matrix X8 (Signal clk Bool)
byteToBits = unpackMatrix . bitwise

addExtend :: (Clock clk)
          => Signal clk Bool
          -> Signal clk Byte
          -> Signal clk Byte
          -> Signal clk U9
addExtend c x y = unsigned x + unsigned y + unsigned c

addCarry :: (Clock clk)
         => Signal clk Bool
         -> Signal clk Byte
         -> Signal clk Byte
         -> (Signal clk Bool, Signal clk Bool, Signal clk Byte)
addCarry c x y = (carry, overflow, unsigned z)
  where
    z = addExtend c x y
    carry = testABit z 8
    overflow = bitNot $ 0x80 .<=. z .&&. z .<. 0x180

subExtend :: (Clock clk)
          => Signal clk Bool
          -> Signal clk Byte
          -> Signal clk Byte
          -> Signal clk U9
subExtend c x y = unsigned x - unsigned y - unsigned (bitNot c)

subCarry :: (Clock clk)
         => Signal clk Bool
         -> Signal clk Byte
         -> Signal clk Byte
         -> (Signal clk Bool, Signal clk Bool, Signal clk Byte)
subCarry c x y = (carry, overflow, unsigned z)
  where
    z = subExtend c x y
    carry = testABit z 8
    overflow = -128 .<=. z .&&. z .<. 128

cpu :: (Clock clk) => CPUIn clk -> (CPUOut clk, CPUDebug clk)
cpu CPUIn{..} = runRTL $ do
    -- State
    s <- newReg Init
    rOp <- newReg BRK
    rArgBuf <- newReg 0x00
    let argAddr = reg rArgBuf `appendS` cpuMemR

    -- Registers
    rA <- newReg 0x00
    rX <- newReg 0x00
    rY <- newReg 0x00
    rSP <- newReg 0xFF
    rPC <- newReg 0x0000 -- To be filled in by Init
    let popTarget = 0x0100 .|. unsigned (reg rSP + 1)
        pushTarget = 0x0100 .|. unsigned (reg rSP)

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

    rNextA <- newReg 0x0000
    rNextW <- newReg Nothing

    let setZN v = do
            fZ := v .==. 0
            fN := v `testABit` 7
    let setA v = do
            setZN v
            rA := v
        setX v = do
            setZN v
            rX := v
        setY v = do
            setZN v
            rY := v

    let write addr val = do
            rNextA := addr
            rNextW := enabledS val
            s := pureS WaitWrite

    let aluA' f aop = case aop of
            Imm -> Op 2 $ Opcode1 act
            ZP -> Op 3 $ OnZP id $ ReadDirect $ const act
            ZP_X -> Op 4 $ OnZP (+ reg rX) $ ReadDirect $ const act
            Abs -> Op 4 $ OnAddr id $ ReadDirect $ const act
            Abs_X -> Op 4 $ OnAddr (+ unsigned (reg rX)) $ ReadDirect $ const act
            Abs_Y -> Op 4 $ OnAddr (+ unsigned (reg rY)) $ ReadDirect $ const act
            Ind_X -> Op 6 $ OnZP (+ reg rX) $ ReadIndirect id act
            Ind_Y -> Op 5 $ OnZP id $ ReadIndirect (+ unsigned (reg rY)) $ act
          where
            act = setA <=< f (reg rA)

        aluA f = aluA' (\a v -> return $ f a v)

    let adc a z = do
            let (c', v', z') = addCarry (reg fC) a z
            fC := c'
            fV := v'
            return z'
        sbc a z = do
            let (c', v', z') = subCarry (reg fC) a z
            fC := c'
            fV := v'
            return z'

        asl z = do
            fC := z `testABit` 7
            setA $ z `shiftL` 1
        lsr z = do
            fC := z `testABit` 0
            setA $ z `shiftR` 1
        rol z = do
            fC := z `testABit` 7
            setA $ z `shiftL` 1 .|. unsigned (reg fC)
        ror z = do
            fC := z `testABit` 0
            setA $ z `shiftR` 1 .|. (unsigned (reg fC) `shiftR` 7)

        cmp x y = do
            fC := x .>=. y
            fZ := x .==. y
            fN := (x - y) .>=. 0x80
            return x

        branch p = Op 2 $ Opcode1 $ \offset -> WHEN p $ do
            rPC := reg rPC + 1 + signed offset


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
                  isRTS = reg rOp .==. pureS RTS
              rPC := mux isRTS (pc', pc' + 1) -- BWAAAAH!
              rNextA := var rPC
              s := pureS Fetch1
          Fetch1 -> do
              rOp := bitwise cpuMemR
              switch (var rOp) $ \k -> case op k of
                  Jam -> return ()
                  Op _ ucode -> case ucode of
                      Opcode0 act -> do
                          act
                          s := pureS Fetch1
                      PopByte{} -> do
                          rNextA := popTarget
                          rSP := reg rSP + 1
                          s := pureS WaitRead
                      _ -> do
                          s := pureS Fetch2
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Halt
          Fetch2 -> do
              switch (reg rOp) $ \k -> case op k of
                  Jam -> return ()
                  Op _ ucode -> case ucode of
                      Opcode1 act -> do
                          act cpuMemR
                          s := pureS Fetch1
                      OnZP toZP _ -> do
                          rNextA := unsigned $ toZP cpuMemR
                          s := pureS Indirect1
                      Opcode2 _ -> do
                          rArgBuf := cpuMemR
                          s := pureS Fetch3
                      OnAddr{} -> do
                          rArgBuf := cpuMemR
                          s := pureS Fetch3
                      _ -> return ()
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Halt
          Fetch3 -> do
              switch (reg rOp) $ \k -> case op k of
                  Jam -> return ()
                  Op _ ucode -> case ucode of
                      Opcode2 act -> do
                          act argAddr
                          s := pureS Fetch1
                      OnAddr toAddr _ -> do
                          rNextA := toAddr argAddr
                          s := pureS Indirect1
                      _ -> return ()
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Halt
          Indirect1 -> do
              switch (reg rOp) $ \k -> case op k of
                  Jam -> return ()
                  Op _ ucode -> case ucode of
                      OnZP _ (ReadDirect act) -> do
                          act (reg rNextA) cpuMemR
                          rNextA := reg rPC
                          s := pureS Fetch1
                      OnAddr _ (ReadDirect act) -> do
                          act (reg rNextA) cpuMemR
                          rNextA := reg rPC
                          s := pureS Fetch1
                      OnZP _ _ -> do
                          rArgBuf := cpuMemR
                          rNextA := reg rNextA + 1
                          s := pureS Indirect2
                      OnAddr _ _ -> do
                          rArgBuf := cpuMemR
                          rNextA := reg rNextA + 1
                          s := pureS Indirect2
                      _ -> return ()
              s := pureS Halt
          Indirect2 -> do
              switch (reg rOp) $ \k -> case op k of
                  Jam -> return ()
                  Op _ ucode -> case ucode of
                      OnZP _ (ReadIndirect toAddr _) -> do
                          rNextA := toAddr argAddr
                          s := pureS WaitRead
                      OnAddr _ (ReadIndirect toAddr _) -> do
                          rNextA := toAddr argAddr
                          s := pureS WaitRead
                      OnZP _ (WriteIndirect toAddr act) -> do
                          v <- act
                          write (toAddr argAddr) v
                      OnAddr _ (WriteIndirect toAddr act) -> do
                          v <- act
                          write (toAddr argAddr) v
                      _ -> return ()
              s := pureS Halt
          WaitRead -> do
              switch (reg rOp) $ \k -> case op k of
                  Jam -> s := pureS Halt
                  Op _ ucode -> case ucode of
                      OnZP _ (ReadIndirect _ act) -> do
                          act cpuMemR
                      PopByte act -> do
                          act cpuMemR
                      _ -> do
                          s := pureS Halt
              rNextA := var rPC
              s := pureS Fetch1
          WaitPushAddr -> do
              rNextA := reg rNextA - 1
              rNextW := enabledS (reg rArgBuf)
              s := pureS WaitWrite
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
        cpuOp = var rOp
        cpuArgBuf = reg rArgBuf
    let cpuA = reg rA
        cpuX = reg rX
        cpuY = reg rY
        cpuSP = reg rSP
        cpuP = flags
        cpuPC = reg rPC

    return (CPUOut{..}, CPUDebug{..})
-}

resetVector :: Addr
resetVector = 0xFFFC

nmiVector :: Addr
nmiVector = 0xFFFA

irqVector :: Addr
irqVector = 0xFFFE
