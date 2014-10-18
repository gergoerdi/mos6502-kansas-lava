{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.Tests where

import MOS6502.Tests.Framework
import MOS6502.Types
import Prelude hiding ((>>=), (>>), return, fail)
import Data.Bits
import Data.Sized.Signed (S8)
import Control.Applicative ((<$>))

allTests :: [Test]
allTests = concat [ branch
                  , jmp
                  , lda
                  , ldx
                  , ldy
                  , [nop]
                  , sta
                  ]
  where
    branch = [ beq, bne, bcs, bcc, bvs, bvc, bmi, bpl ]
    jmp = [ jmp_abs, jmp_ind ]
    lda = [ lda_imm, lda_zp, lda_zp_x, lda_abs, lda_abs_x, lda_abs_y, lda_ind_x, lda_ind_y ]
    ldx = [ ldx_imm, ldx_zp, ldx_zp_y, ldx_abs, ldx_abs_y]
    ldy = [ ldy_imm, ldy_zp, ldy_zp_x, ldy_abs, ldy_abs_x]
    sta = [ sta_zp, sta_zp_x, sta_abs, sta_abs_x, sta_abs_y, sta_ind_x, sta_ind_y ]


ifThenElse :: Bool -> a -> a -> a
ifThenElse True thn _els = thn
ifThenElse False _thn els = els

(>>=) :: TestM from int a -> (a -> TestM int to b) -> TestM from to b
(>>=) = (:>>=)

(>>) :: TestM from int a -> TestM int to b -> TestM from to b
m1 >> m2 = m1 >>= const m2

return :: a -> TestM from from a
return = Return

fail :: String -> TestM from to a
fail = error

checkFlags :: TestM After After Byte -> TestM After After Byte
checkFlags query = do
    b <- query
    flags <- after statusFlags
    let z = flags `testBit` 6
        n = flags `testBit` 0
    assert "Z flag is correctly set" $ z == (b == 0)
    assert "N flag is correctly set" $ n == (b `testBit` 7)
    return b

derefZP :: Byte -> TestM Before Before Addr
derefZP zp = do
    lo <- before $ memZP zp
    hi <- before $ memZP (zp + 1)
    return $ toAddr lo hi

deref :: Addr -> TestM Before Before Addr
deref addr = do
    lo <- before $ mem addr
    hi <- before $ mem nextAddr
    return $ toAddr lo hi
  where
    loAddr :: Byte
    hiAddr :: Byte
    (loAddr, hiAddr) = (fromIntegral addr, fromIntegral (addr `shiftR` 8))
    nextAddr = toAddr (loAddr + 1) hiAddr

branch :: String -> Byte -> (Byte -> Bool) -> Test
branch name opcode takeBranch = op1 name $ \offset -> do
    let offset' = fromIntegral offset :: S8
    taken <- takeBranch <$> before statusFlags
    pc <- before regPC
    -- TODO: extra cycle for page boundary
    execute opcode $ if taken then 3 else 2
    pc' <- after regPC
    assertEq (if taken then "Branch taken" else "Branch not taken") pc' $
      if taken then pc + 2 + fromIntegral offset' else pc + 2

beq :: Test
beq = branch "BEQ" 0xF0 (`testBit` 6)

bne :: Test
bne = branch "BNE" 0xD0 $ not . (`testBit` 6)

bcs :: Test
bcs = branch "BCS" 0xB0 (`testBit` 7)

bcc :: Test
bcc = branch "BCC" 0x90 $ not . (`testBit` 7)

bvs :: Test
bvs = branch "BVS" 0x70 (`testBit` 1)

bvc :: Test
bvc = branch "BVS" 0x50 $ not . (`testBit` 1)

bmi :: Test
bmi = branch "BMI" 0x30 (`testBit` 0)

bpl :: Test
bpl = branch "BPL" 0x10 $ not . (`testBit` 0)

nop :: Test
nop = op0 "NOP" $ do
    execute 0xEA 2

lda_imm :: Test
lda_imm = op1 "LDA imm" $ \imm -> do
    execute 0xA9 2
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == imm

lda_zp :: Test
lda_zp = op1 "LDA zp" $ \zp -> do
    b <- before $ memZP zp
    execute 0xA5 3
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

lda_zp_x :: Test
lda_zp_x = op1 "LDA zp,X" $ \zp -> do
    x <- before regX
    b <- before $ memZP (zp + x)
    execute 0xB5 4
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

lda_abs :: Test
lda_abs = op2 "LDA abs" $ \addr -> do
    b <- before $ mem addr
    execute 0xAD 4
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

lda_abs_x :: Test
lda_abs_x = op2 "LDA abs,X" $ \addr -> do
    x <- before regX
    let (addr', bankFault) = offset addr x
    b <- before $ mem addr'
    execute 0xBD $ if bankFault then 5 else 4
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

lda_abs_y :: Test
lda_abs_y = op2 "LDA abs,Y" $ \addr -> do
    y <- before regY
    let (addr', bankFault) = offset addr y
    b <- before $ mem addr'
    execute 0xB9 $ if bankFault then 5 else 4
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

lda_ind_x :: Test
lda_ind_x = op1 "LDA (zp,X)" $ \zp -> do
    x <- before regX
    addr <- derefZP $ zp + x
    b <- before $ mem addr
    execute 0xA1 6
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

lda_ind_y :: Test
lda_ind_y = op1 "LDA (zp),Y" $ \zp -> do
    y <- before regY
    addr <- derefZP $ zp
    let (addr', bankFault) = offset addr y
    b <- before $ mem addr'
    execute 0xB1 $ if bankFault then 6 else 5
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

ldx_imm :: Test
ldx_imm = op1 "LDX imm" $ \imm -> do
    execute 0xA2 2
    x' <- checkFlags $ after regX
    assert "X is correctly set" $ x' == imm

ldx_zp :: Test
ldx_zp = op1 "LDX zp" $ \zp -> do
    b <- before $ memZP zp
    execute 0xA6 3
    x' <- checkFlags $ after regX
    assert "X is correctly set" $ x' == b

ldx_zp_y :: Test
ldx_zp_y = op1 "LDX zp,Y" $ \zp -> do
    y <- before regY
    b <- before $ memZP (zp + y)
    execute 0xB6 4
    x' <- checkFlags $ after regX
    assert "X is correctly set" $ x' == b

ldx_abs :: Test
ldx_abs = op2 "LDX abs" $ \addr -> do
    b <- before $ mem addr
    execute 0xAE 4
    x' <- checkFlags $ after regX
    assert "X is correctly set" $ x' == b

ldx_abs_y :: Test
ldx_abs_y = op2 "LDX abs,Y" $ \addr -> do
    y <- before regY
    let (addr', bankFault) = offset addr y
    b <- before $ mem addr'
    execute 0xBE $ if bankFault then 5 else 4
    x' <- checkFlags $ after regX
    assert "X is correctly set" $ x' == b

ldy_imm :: Test
ldy_imm = op1 "LDY imm" $ \imm -> do
    execute 0xA0 2
    y' <- checkFlags $ after regY
    assert "Y is correctly set" $ y' == imm

ldy_zp :: Test
ldy_zp = op1 "LDY zp" $ \zp -> do
    b <- before $ memZP zp
    execute 0xA4 3
    y' <- checkFlags $ after regY
    assert "Y is correctly set" $ y' == b

ldy_zp_x :: Test
ldy_zp_x = op1 "LDY zp,X" $ \zp -> do
    x <- before regX
    b <- before $ memZP (zp + x)
    execute 0xB4 4
    y' <- checkFlags $ after regY
    assert "Y is correctly set" $ y' == b

ldy_abs :: Test
ldy_abs = op2 "LDY abs" $ \addr -> do
    b <- before $ mem addr
    execute 0xAC 4
    y' <- checkFlags $ after regY
    assert "Y is correctly set" $ y' == b

ldy_abs_x :: Test
ldy_abs_x = op2 "LDY abs,X" $ \addr -> do
    x <- before regX
    let (addr', bankFault) = offset addr x
    b <- before $ mem addr'
    execute 0xBC $ if bankFault then 5 else 4
    y' <- checkFlags $ after regY
    assert "Y is correctly set" $ y' == b

sta_zp :: Test
sta_zp = op1 "STA zp" $ \zp -> do
    a <- before regA
    execute 0x85 3
    b' <- after $ memZP zp
    assert "B[ZP]" $ b' == a

sta_zp_x :: Test
sta_zp_x = op1 "STA zp,X" $ \zp -> do
    a <- before regA
    x <- before regX
    execute 0x95 4
    b' <- after $ memZP (zp + x)
    assert "B[@@,X]" $ b' == a

sta_abs :: Test
sta_abs = op2 "STA abs" $ \addr -> do
    a <- before regA
    execute 0x8D 5
    b' <- after $ mem addr
    assert "B[@@@@]" $ b' == a

sta_abs_x :: Test
sta_abs_x = op2 "STA abs,X" $ \addr -> do
    a <- before regA
    x <- before regX
    execute 0x9D 5
    b' <- after $ mem (addr + fromIntegral x)
    assert "B[@@@@,X]" $ b' == a

sta_abs_y :: Test
sta_abs_y = op2 "STA abs,Y" $ \addr -> do
    a <- before regA
    y <- before regY
    execute 0x99 5
    b' <- after $ mem (addr + fromIntegral y)
    assert "B[@@@@,Y]" $ b' == a

sta_ind_x :: Test
sta_ind_x = op1 "STA (zp,X)" $ \zp -> do
    a <- before regA
    x <- before regX
    addr <- derefZP $ zp + x
    execute 0x81 6
    b' <- after $ mem addr
    assert "B[(@@,X)]" $ b' == a

sta_ind_y :: Test
sta_ind_y = op1 "STA (zp),Y" $ \zp -> do
    a <- before regA
    y <- before regY
    addr <- derefZP zp
    execute 0x91 6
    let (addr', _) = offset addr y
    b' <- after $ mem addr'
    assert "B[(@@),Y]" $ b' == a

jmp_abs :: Test
jmp_abs = op2 "JMP abs" $ \addr -> do
    execute 0x4C 3
    pc' <- after regPC
    assert "PC" $ pc' == addr

jmp_ind :: Test
jmp_ind = op2 "JMP ind" $ \addr -> do
    addr' <- deref addr
    execute 0x6C 5
    pc' <- after regPC
    assert "PC" $ pc' == addr'
