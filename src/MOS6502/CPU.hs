{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.CPU where

import MOS6502.Opcodes
import MOS6502.Types
import MOS6502.Utils

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
         -> (Signal clk Bool, Signal clk Byte)
addCarry c x y = (testABit z 8, unsigned z)
  where
    z = addExtend c x y

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
         -> (Signal clk Bool, Signal clk Byte)
subCarry c x y = (testABit z 8, unsigned z)
  where
    z = subExtend c x y

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
            fN := v .>=. 0x80
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

    let adc a v = do
            let (c', v') = addCarry (reg fC) a v
            fC := c'
            fV := v' `testABit` 7
            return v'
        sbc a v = do
            let (c', v') = subCarry (reg fC) a v
            fC := c'
            fV := bitNot $ v' `testABit` 7
            return v'

        cmp x y = do
            fC := x .>=. y
            fZ := x .==. y
            fN := (x - y) .>=. 0x80
            return x

        branch p = Op 2 $ Opcode1 $ \offset -> WHEN p $ do
            rPC := reg rPC + 1 + signed offset


    let op LDA_Imm = aluA (\_a v -> v) Imm
        op LDA_ZP = aluA (\_a v -> v) ZP
        op LDA_ZP_X = aluA (\_a v -> v) ZP_X
        op LDA_Abs = aluA (\_a v -> v) Abs
        op LDA_Abs_X = aluA (\_a v -> v) Abs_X
        op LDA_Abs_Y = aluA (\_a v -> v) Abs_Y
        op LDA_Ind_X = aluA (\_a v -> v) Ind_X
        op LDA_Ind_Y = aluA (\_a v -> v) Ind_Y

        op LDX_Imm = Op 2 $ Opcode1 $ \imm -> do
            setX imm
        op LDX_ZP = Op 3 $ OnZP id $ ReadDirect $ \ _ v -> do
            setX v
        op LDX_ZP_Y = Op 4 $ OnZP (+ reg rY) $ ReadDirect $ \ _ v -> do
            setX v
        op LDX_Abs = Op 4 $ OnAddr id $ ReadDirect $ \ _ v -> do
            setX v
        op LDX_Abs_Y = Op 4 $ OnAddr (+ unsigned (reg rY)) $ ReadDirect $ \ _ v -> do
            setX v

        op LDY_Imm = Op 2 $ Opcode1 $ \imm -> do
            setY imm
        op LDY_ZP = Op 3 $ OnZP id $ ReadDirect $ \ _ v -> do
            setY v
        op LDY_ZP_X = Op 4 $ OnZP (+ reg rX) $ ReadDirect $ \ _ v -> do
            setY v
        op LDY_Abs = Op 4 $ OnAddr id $ ReadDirect $ \ _ v -> do
            setY v
        op LDY_Abs_X = Op 4 $ OnAddr (+ unsigned (reg rX)) $ ReadDirect $ \ _ v -> do
            setY v

        op STA_ZP = Op 3 $ Opcode1 $ \zp -> do
            write (unsigned zp) (reg rA)
        op STA_ZP_X = Op 4 $ Opcode1 $ \zp -> do
            write (unsigned $ zp + reg rX) (reg rA)
        op STA_Abs = Op 4 $ Opcode2 $ \addr -> do
            write addr (reg rA)
        op STA_Abs_X = Op 5 $ Opcode2 $ \addr -> do
            let addr' = addr + unsigned (reg rX)
            write addr' (reg rA)
        op STA_Abs_Y = Op 5 $ Opcode2 $ \addr -> do
            let addr' = addr + unsigned (reg rY)
            write addr' (reg rA)
        op STA_Ind_X = Op 6 $ OnZP (+ reg rX) $ WriteIndirect id $ do
            return $ reg rA
        op STA_Ind_Y = Op 6 $ OnZP id $ WriteIndirect (+ unsigned (reg rY)) $ do
            return $ reg rA

        op STX_ZP = Op 3 $ Opcode1 $ \zp -> do
            write (unsigned zp) (reg rX)
        op STX_ZP_Y = Op 4 $ Opcode1 $ \zp -> do
            write (unsigned $ zp + reg rY) (reg rX)
        op STX_Abs = Op 4 $ Opcode2 $ \addr -> do
            write addr (reg rX)

        op STY_ZP = Op 3 $ Opcode1 $ \zp -> do
            write (unsigned zp) (reg rY)
        op STY_ZP_X = Op 4 $ Opcode1 $ \zp -> do
            write (unsigned $ zp + reg rX) (reg rY)
        op STY_Abs = Op 4 $ Opcode2 $ \addr -> do
            write addr (reg rY)

        op INX = Op 2 $ Opcode0 $ do
            setX $ reg rX + 1
        op DEX = Op 2 $ Opcode0 $ do
            setX $ reg rX - 1
        op INY = Op 2 $ Opcode0 $ do
            setY $ reg rY + 1
        op DEY = Op 2 $ Opcode0 $ do
            setY $ reg rY - 1

        op TAX = Op 2 $ Opcode0 $ setX $ reg rA
        op TXA = Op 2 $ Opcode0 $ setA $ reg rX
        op TAY = Op 2 $ Opcode0 $ setY $ reg rA
        op TYA = Op 2 $ Opcode0 $ setA $ reg rY

        op TSX = Op 2 $ Opcode0 $ setX $ reg rSP
        op TXS = Op 2 $ Opcode0 $ rSP := reg rX

        op INC_ZP = Op 5 $ OnZP id $ ReadDirect $ \addr v -> do
            let v' = v + 1
            setZN v'
            write addr v'
        op INC_ZP_X = Op 6 $ OnZP (+ reg rX) $ ReadDirect $ \addr v -> do
            let v' = v + 1
            setZN v'
            write addr v'
        op INC_Abs = Op 6 $ OnAddr id $ ReadDirect $ \addr v -> do
            let v' = v + 1
            setZN v'
            write addr v'
        op INC_Abs_X = Op 7 $ OnAddr (+ unsigned (reg rX)) $ ReadDirect $ \addr v -> do
            let v' = v + 1
            setZN v'
            write addr v'

        op DEC_ZP = Op 5 $ OnZP id $ ReadDirect $ \addr v -> do
            let v' = v - 1
            setZN v'
            write addr v'
        op DEC_ZP_X = Op 6 $ OnZP (+ reg rX) $ ReadDirect $ \addr v -> do
            let v' = v - 1
            setZN v'
            write addr v'
        op DEC_Abs = Op 6 $ OnAddr id $ ReadDirect $ \addr v -> do
            let v' = v - 1
            setZN v'
            write addr v'
        op DEC_Abs_X = Op 7 $ OnAddr (+ unsigned (reg rX)) $ ReadDirect $ \addr v -> do
            let v' = v - 1
            setZN v'
            write addr v'

        op AND_Imm = aluA (.&.) Imm
        op AND_ZP = aluA (.&.) ZP
        op AND_ZP_X = aluA (.&.) ZP_X
        op AND_Abs = aluA (.&.) Abs
        op AND_Abs_X = aluA (.&.) Abs_X
        op AND_Abs_Y = aluA (.&.) Abs_Y
        op AND_Ind_X = aluA (.&.) Ind_X
        op AND_Ind_Y = aluA (.&.) Ind_Y

        op ORA_Imm = aluA (.|.) Imm
        op ORA_ZP = aluA (.|.) ZP
        op ORA_ZP_X = aluA (.|.) ZP_X
        op ORA_Abs = aluA (.|.) Abs
        op ORA_Abs_X = aluA (.|.) Abs_X
        op ORA_Abs_Y = aluA (.|.) Abs_Y
        op ORA_Ind_X = aluA (.|.) Ind_X
        op ORA_Ind_Y = aluA (.|.) Ind_Y

        op EOR_Imm = aluA xor Imm
        op EOR_ZP = aluA xor ZP
        op EOR_ZP_X = aluA xor ZP_X
        op EOR_Abs = aluA xor Abs
        op EOR_Abs_X = aluA xor Abs_X
        op EOR_Abs_Y = aluA xor Abs_Y
        op EOR_Ind_X = aluA xor Ind_X
        op EOR_Ind_Y = aluA xor Ind_Y

        op ADC_Imm = aluA' adc Imm
        op ADC_ZP = aluA' adc ZP
        op ADC_ZP_X = aluA' adc ZP_X
        op ADC_Abs = aluA' adc Abs
        op ADC_Abs_X = aluA' adc Abs_X
        op ADC_Abs_Y = aluA' adc Abs_Y
        op ADC_Ind_X = aluA' adc Ind_X
        op ADC_Ind_Y = aluA' adc Ind_Y

        op SBC_Imm = aluA' sbc Imm
        op SBC_ZP = aluA' sbc ZP
        op SBC_ZP_X = aluA' sbc ZP_X
        op SBC_Abs = aluA' sbc Abs
        op SBC_Abs_X = aluA' sbc Abs_X
        op SBC_Abs_Y = aluA' sbc Abs_Y
        op SBC_Ind_X = aluA' sbc Ind_X
        op SBC_Ind_Y = aluA' sbc Ind_Y

        op CMP_Imm = aluA' cmp Imm
        op CMP_ZP = aluA' cmp ZP
        op CMP_ZP_X = aluA' cmp ZP_X
        op CMP_Abs = aluA' cmp Abs
        op CMP_Abs_X = aluA' cmp Abs_X
        op CMP_Abs_Y = aluA' cmp Abs_Y
        op CMP_Ind_X = aluA' cmp Ind_X
        op CMP_Ind_Y = aluA' cmp Ind_Y

        op CPX_Imm = Op 2 $ Opcode1 $ void . cmp (reg rX)
        op CPX_ZP = Op 3 $ OnZP id $ ReadDirect . const $ void . cmp (reg rX)
        op CPX_Abs = Op 4 $ OnAddr id $ ReadDirect . const $ void . cmp (reg rX)

        op CPY_Imm = Op 2 $ Opcode1 $ void . cmp (reg rY)
        op CPY_ZP = Op 3 $ OnZP id $ ReadDirect . const $ void . cmp (reg rY)
        op CPY_Abs = Op 4 $ OnAddr id $ ReadDirect . const $ void . cmp (reg rY)

        op CLC = Op 2 $ Opcode0 $ fC := low
        op SEC = Op 2 $ Opcode0 $ fC := high
        op CLI = Op 2 $ Opcode0 $ fI := low
        op SEI = Op 2 $ Opcode0 $ fI := high
        op CLV = Op 2 $ Opcode0 $ fV := low
        op CLD = Op 2 $ Opcode0 $ fD := low
        op SED = Op 2 $ Opcode0 $ fD := high

        -- TODO: flags
        op ASL_A = Op 2 $ Opcode0 $ do
            fC := reg rA .>=. 0x80
            setA $ reg rA `shiftL` 1

        -- TODO: flags
        op LSR_A = Op 2 $ Opcode0 $ do
            setA $ reg rA `rotateR` 1

        op JMP_Abs = Op 3 $ Opcode2 $ \addr -> do
            rPC := addr
        op JMP_Ind = Op 5 $ Opcode2 $ \addr -> do
            rNextA := addr
            s := pureS FetchVector1

        op JSR = Op 6 $ Opcode2 $ \addr -> do
            rNextA := pushTarget
            rNextW := enabledS (unsigned $ reg rPC `shiftR` 8)
            rArgBuf := unsigned (reg rPC)
            rSP := reg rSP - 2
            s := pureS WaitPushAddr
            rPC := addr

        op RTS = Op 6 $ Opcode0 $ do
            rSP := reg rSP + 2
            rNextA := popTarget
            s := pureS FetchVector1

        op PHA = Op 3 $ Opcode0 $ do
            write pushTarget (reg rA)
            rSP := reg rSP - 1
        op PLA = Op 4 $ PopByte setA

        op PHP = Op 3 $ Opcode0 $ do
            write pushTarget flags
            rSP := reg rSP - 1
        op PLP = Op 4 $ PopByte setFlags

        op BEQ = branch (reg fZ)
        op BNE = branch (bitNot $ reg fZ)
        op BCS = branch (reg fC)
        op BCC = branch (bitNot $ reg fC)

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
        cpuOp = reg rOp
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
