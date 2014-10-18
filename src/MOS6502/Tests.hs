{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.Tests where

import MOS6502.Tests.Framework
import MOS6502.Types
import Prelude hiding ((>>=), (>>), return, fail)
import Data.Bits

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

nop :: Test
nop = op0 $ do
    execute 0xEA 2

lda_imm :: Test
lda_imm = op1 $ \imm -> do
    execute 0xA9 2
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == imm

lda_zp :: Test
lda_zp = op1 $ \zp -> do
    b <- before $ memZP zp
    execute 0xA5 3
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

lda_zp_x :: Test
lda_zp_x = op1 $ \zp -> do
    x <- before regX
    b <- before $ memZP (zp + x)
    execute 0xB5 4
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

lda_abs :: Test
lda_abs = op2 $ \addr -> do
    b <- before $ mem addr
    execute 0xAD 4
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

lda_abs_x :: Test
lda_abs_x = op2 $ \addr -> do
    x <- before regX
    let (addr', bankFault) = offset addr x
    b <- before $ mem addr'
    execute 0xBD $ if bankFault then 5 else 4
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

lda_abs_y :: Test
lda_abs_y = op2 $ \addr -> do
    y <- before regY
    let (addr', bankFault) = offset addr y
    b <- before $ mem addr'
    execute 0xB9 $ if bankFault then 5 else 4
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

lda_ind_x :: Test
lda_ind_x = op1 $ \zp -> do
    x <- before regX
    addr <- derefZP $ zp + x -- before $ derefZP $ zp + x
    b <- before $ mem addr
    execute 0xA1 6
    a' <- checkFlags $ after regA
    assert "A is correctly set" $ a' == b

sta_zp :: Test
sta_zp = op1 $ \zp -> do
    a <- before regA
    execute 0x85 3
    b' <- after $ memZP zp
    assert "B[ZP]" $ b' == a

sta_ind_x :: Test
sta_ind_x = op1 $ \zp -> do
    a <- before regA
    x <- before regX
    addr <- derefZP $ zp + x
    execute 0x81 6
    b' <- after $ mem addr
    assert "B[(ZP,X)]" $ b' == a

sta_ind_y :: Test
sta_ind_y = op1 $ \zp -> do
    a <- before regA
    y <- before regY
    addr <- derefZP zp
    execute 0x91 6
    let (addr', _) = offset addr y
    b' <- after $ mem addr'
    assert "B[(ZP),Y]" $ b' == a

jmp_abs :: Test
jmp_abs = op2 $ \addr -> do
    execute 0x4C 3
    pc' <- after regPC
    assert "PC" $ pc' == addr
