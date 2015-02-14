{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.Tests where

import MOS6502.Tests.Framework
import MOS6502.Types
import Data.Bits hiding (bit)
import Data.Sized.Signed (S8)
import Control.Applicative

allTests :: [Test]
allTests = concat [ branch
                  , jmp
                  , lda
                  , ldx
                  , ldy
                  , [nop]
                  , sta
                  , bit
                  , cmp
                  , transfer
                  ]
  where
    branch = [ beq, bne, bcs, bcc, bvs, bvc, bmi, bpl ]
    jmp = [ jmp_abs, jmp_ind ]
    lda = [ lda_imm, lda_zp, lda_zp_x, lda_abs, lda_abs_x, lda_abs_y, lda_ind_x, lda_ind_y ]
    ldx = [ ldx_imm, ldx_zp, ldx_zp_y, ldx_abs, ldx_abs_y]
    ldy = [ ldy_imm, ldy_zp, ldy_zp_x, ldy_abs, ldy_abs_x]
    sta = [ sta_zp, sta_zp_x, sta_abs, sta_abs_x, sta_abs_y, sta_ind_x, sta_ind_y ]
    transfer = [ inx, dex, iny, dey, tax, txa, tay, tya, txs, tsx ]

nop :: Test
nop = op0 "NOP" $ do
    execute0 0xEA 2

checkFlags :: TestM (Obs Byte) -> TestM (Obs Byte)
checkFlags query = do
    b <- query
    flags <- observe statusFlags
    let z = (`testBit` 1) <$> flags
        n = (`testBit` 7) <$> flags
    assertEq "Z flag is correctly set" z ((==) <$> b <*> 0)
    assertEq "N flag is correctly set" n ((`testBit` 7) <$> b)
    return b

cmp :: [Test]
cmp = [ {- cmp_imm, cmp_zp, cmp_zp_z, cmp_abs, cmp_abs_x, cmp_abs_y, cmp_ind_x, -} cmp_ind_y ]
  where
    cmp_ind_y = op1 "CMP (zp),Y" $ \zp -> do
        y <- observe regY
        addr <- derefZP $ pure zp
        let (addr', bankFault) = offset addr y
        b <- observe $ mem addr'
        cmp b $ execute1 0xD1 zp (5 + costly bankFault)

    cmp b execute = do
        a <- observe regA
        execute
        checkFlags $ return $ a - b
        return ()

bit :: [Test]
bit = [ bit_zp, bit_abs ]
  where
    bit_zp = op1 "BIT zp" $ \zp -> do
        b <- observe $ memZP (pure zp)
        bit b (execute1 0x24 zp 3)

    bit_abs = op2 "BIT abs" $ \addr -> do
        b <- observe $ mem (pure addr)
        bit b (execute2 0x2C addr 4)

    bit b execute = do
        a <- observe regA
        let a' = liftA2 (.&.) a b
        execute
        flags <- observe statusFlags
        let z = (`testBit` 1) <$> flags
            n = (`testBit` 7) <$> flags
            v = (`testBit` 6) <$> flags
        assertEq "Z flag is correctly set" z ((==) <$> a' <*> 0)
        assertEq "N flag is correctly copied" n ((`testBit` 7) <$> b)
        assertEq "V flag is correctly copied" v ((`testBit` 6) <$> b)

transfer :: String -> Byte -> Reg -> Reg -> (Byte -> Byte) -> Test
transfer label opcode from to fun = op0 label $ do
    old <- observe $ Reg from
    execute0 opcode 2
    new <- checkFlags $ observe $ Reg to
    assertEq (unwords [show from, "->", show to]) new (fun <$> old)

costly :: Obs Bool -> Obs Int
costly b = (\b -> if b then 1 else 0) <$> b

branch :: String -> Byte -> (Byte -> Bool) -> Test
branch name opcode takeBranch = op1 name $ \offset -> do
    let offset' = fromIntegral offset :: S8
    taken <- fmap takeBranch <$> observe statusFlags
    pc <- observe regPC
    -- TODO: extra cycle for page boundary
    execute1 opcode offset (2 + costly taken)
    pc' <- observe regPC
    assertEq "Branch correctly taken" pc' $
      pc + 2 + (cond <$> taken <*> fromIntegral offset' <*> 0)
  where
    cond b x y = if b then x else y

derefZP :: Obs Byte -> TestM (Obs Addr)
derefZP zp = do
    lo <- observe $ memZP zp
    hi <- observe $ memZP (zp + 1)
    return $ toAddr <$> lo <*> hi

deref :: Obs Addr -> TestM (Obs Addr)
deref addr = do
    lo <- observe $ mem addr
    hi <- observe $ mem $ nextAddr <$> addr
    return $ toAddr <$> lo <*> hi
  where
    nextAddr addr = let (lo, hi) = splitAddr addr
                    in toAddr (lo + 1) hi

and_imm :: Test
and_imm = binALU_imm "AND" 0x29 (.&.)

lda_imm :: Test
lda_imm = binALU_imm "LDA" 0xA9 (\ _ -> id)

lda_zp :: Test
lda_zp = binALU_zp "LDA" 0xA5 (\ _ -> id)

binALU_zp :: String -> Byte -> (Byte -> Byte -> Byte) -> Test
binALU_zp name opcode fun = op1 (unwords [name, "zp"]) $ \zp -> do
    a <- observe regA
    b <- observe $ memZP (pure zp)
    execute1 opcode zp 3
    a' <- checkFlags $ observe regA
    assertEq "A is correctly set" a' (fun <$> a <*> b)

binALU_imm :: String -> Byte -> (Byte -> Byte -> Byte) -> Test
binALU_imm name opcode fun = op1 (unwords [name, "imm"]) $ \imm -> do
    a <- observe regA
    execute1 opcode imm 2
    a' <- checkFlags $ observe regA
    assertEq "A is updated" a' (fun <$> a <*> pure imm)

beq :: Test
beq = branch "BEQ" 0xF0 (`testBit` 1)

bne :: Test
bne = branch "BNE" 0xD0 $ not . (`testBit` 1)

bcs :: Test
bcs = branch "BCS" 0xB0 (`testBit` 0)

bcc :: Test
bcc = branch "BCC" 0x90 $ not . (`testBit` 0)

bvs :: Test
bvs = branch "BVS" 0x70 (`testBit` 6)

bvc :: Test
bvc = branch "BVS" 0x50 $ not . (`testBit` 6)

bmi :: Test
bmi = branch "BMI" 0x30 (`testBit` 7)

bpl :: Test
bpl = branch "BPL" 0x10 $ not . (`testBit` 7)

dex :: Test
dex = transfer "DEX" 0xCA X X (subtract 1)

inx :: Test
inx = transfer "INX" 0xE8 X X (+ 1)

dey :: Test
dey = transfer "DEY" 0x88 Y Y (subtract 1)

iny :: Test
iny = transfer "INY" 0xC8 Y Y (+ 1)

tax :: Test
tax = transfer "TAX" 0xAA A X id

tay :: Test
tay = transfer "TAY" 0xA8 A Y id

txa :: Test
txa = transfer "TAX" 0x8A X A id

tya :: Test
tya = transfer "TYA" 0x98 Y A id

txs :: Test
txs = op0 "TXS" $ do
    x <- observe regX
    execute0 0x9A 2
    sp <- checkFlags $ observe regSP
    assertEq "X->SP" sp x

tsx :: Test
tsx = op0 "TSX" $ do
    sp <- observe $ Reg SP
    execute0 0xBA 2
    x <- checkFlags $ observe regX
    assertEq "SP->X" x sp

lda_zp_x :: Test
lda_zp_x = op1 "LDA zp,X" $ \zp -> do
    x <- observe regX
    b <- observe $ memZP (pure zp + x)
    execute1 0xB5 zp 4
    a' <- checkFlags $ observe regA
    assertEq "A is correctly set" a' b

lda_abs :: Test
lda_abs = op2 "LDA abs" $ \addr -> do
    b <- observe $ mem (pure addr)
    execute2 0xAD addr 4
    a' <- checkFlags $ observe regA
    assertEq "A is correctly set" a' b

lda_abs_x :: Test
lda_abs_x = op2 "LDA abs,X" $ \addr -> do
    x <- observe regX
    let (addr', bankFault) = offset (pure addr) x
    b <- observe $ mem addr'
    execute2 0xBD addr (4 + costly bankFault)
    a' <- checkFlags $ observe regA
    assertEq "A is correctly set" a' b

lda_abs_y :: Test
lda_abs_y = op2 "LDA abs,Y" $ \addr -> do
    y <- observe regY
    let (addr', bankFault) = offset (pure addr) y
    b <- observe $ mem addr'
    execute2 0xB9 addr (4 + costly bankFault)
    a' <- checkFlags $ observe regA
    assertEq "A is correctly set" a' b

lda_ind_x :: Test
lda_ind_x = op1 "LDA (zp,X)" $ \zp -> do
    x <- observe regX
    addr <- derefZP $ pure zp + x
    b <- observe $ mem addr
    execute1 0xA1 zp 6
    a' <- checkFlags $ observe regA
    assertEq "A is correctly set" a' b

lda_ind_y :: Test
lda_ind_y = op1 "LDA (zp),Y" $ \zp -> do
    y <- observe regY
    addr <- derefZP $ pure zp
    let (addr', bankFault) = offset addr y
    b <- observe $ mem addr'
    execute1 0xB1 zp (5 + costly bankFault)
    a' <- checkFlags $ observe regA
    assertEq "A is correctly set" a' b

ldx_imm :: Test
ldx_imm = op1 "LDX imm" $ \imm -> do
    execute1 0xA2 imm 2
    x' <- checkFlags $ observe regX
    assertEq "X is correctly set" x' (pure imm)

ldx_zp :: Test
ldx_zp = op1 "LDX zp" $ \zp -> do
    b <- observe $ memZP (pure zp)
    execute1 0xA6 zp 3
    x' <- checkFlags $ observe regX
    assertEq "X is correctly set" x' b

ldx_zp_y :: Test
ldx_zp_y = op1 "LDX zp,Y" $ \zp -> do
    y <- observe regY
    b <- observe $ memZP (pure zp + y)
    execute1 0xB6 zp 4
    x' <- checkFlags $ observe regX
    assertEq "X is correctly set" x' b

ldx_abs :: Test
ldx_abs = op2 "LDX abs" $ \addr -> do
    b <- observe $ mem (pure addr)
    execute2 0xAE addr 4
    x' <- checkFlags $ observe regX
    assertEq "X is correctly set" x' b

ldx_abs_y :: Test
ldx_abs_y = op2 "LDX abs,Y" $ \addr -> do
    y <- observe regY
    let (addr', bankFault) = offset (pure addr) y
    b <- observe $ mem addr'
    execute2 0xBE addr (4 + costly bankFault)
    x' <- checkFlags $ observe regX
    assertEq "X is correctly set" x' b

ldy_imm :: Test
ldy_imm = op1 "LDY imm" $ \imm -> do
    execute1 0xA0 imm 2
    y' <- checkFlags $ observe regY
    assertEq "Y is correctly set" y' (pure imm)

ldy_zp :: Test
ldy_zp = op1 "LDY zp" $ \zp -> do
    b <- observe $ memZP (pure zp)
    execute1 0xA4 zp 3
    y' <- checkFlags $ observe regY
    assertEq "Y is correctly set" y' b

ldy_zp_x :: Test
ldy_zp_x = op1 "LDY zp,X" $ \zp -> do
    x <- observe regX
    b <- observe $ memZP (pure zp + x)
    execute1 0xB4 zp 4
    y' <- checkFlags $ observe regY
    assertEq "Y is correctly set" y' b

ldy_abs :: Test
ldy_abs = op2 "LDY abs" $ \addr -> do
    b <- observe $ mem (pure addr)
    execute2 0xAC addr 4
    y' <- checkFlags $ observe regY
    assertEq "Y is correctly set" y' b

ldy_abs_x :: Test
ldy_abs_x = op2 "LDY abs,X" $ \addr -> do
    x <- observe regX
    let (addr', bankFault) = offset (pure addr) x
    b <- observe $ mem addr'
    execute2 0xBC addr (4 + costly bankFault)
    y' <- checkFlags $ observe regY
    assertEq "Y is correctly set" y' b

sta_zp :: Test
sta_zp = op1 "STA zp" $ \zp -> do
    a <- observe regA
    execute1 0x85 zp 3
    b' <- observe $ memZP (pure zp)
    assertEq "B[ZP]" b' a

sta_zp_x :: Test
sta_zp_x = op1 "STA zp,X" $ \zp -> do
    a <- observe regA
    x <- observe regX
    execute1 0x95 zp 4
    b' <- observe $ memZP (pure zp + x)
    assertEq "B[@@,X]" b' a

sta_abs :: Test
sta_abs = op2 "STA abs" $ \addr -> do
    a <- observe regA
    execute2 0x8D addr 5
    b' <- observe $ mem (pure addr)
    assertEq "B[@@@@]" b' a

sta_abs_x :: Test
sta_abs_x = op2 "STA abs,X" $ \addr -> do
    a <- observe regA
    x <- observe regX
    execute2 0x9D addr 5
    b' <- observe $ mem (pure addr + (fromIntegral <$> x))
    assertEq "B[@@@@,X]" b' a

sta_abs_y :: Test
sta_abs_y = op2 "STA abs,Y" $ \addr -> do
    a <- observe regA
    y <- observe regY
    execute2 0x99 addr 5
    b' <- observe $ mem (pure addr + (fromIntegral <$> y))
    assertEq "B[@@@@,Y]" b' a

sta_ind_x :: Test
sta_ind_x = op1 "STA (zp,X)" $ \zp -> do
    a <- observe regA
    x <- observe regX
    addr <- derefZP $ pure zp + x
    execute1 0x81 zp 6
    b' <- observe $ mem addr
    assertEq "B[(@@,X)]" b' a

sta_ind_y :: Test
sta_ind_y = op1 "STA (zp),Y" $ \zp -> do
    a <- observe regA
    y <- observe regY
    addr <- derefZP $ pure zp
    execute1 0x91 zp 6
    let (addr', _) = offset addr y
    b' <- observe $ mem addr'
    assertEq "B[(@@),Y]" b' a

jmp_abs :: Test
jmp_abs = op2 "JMP abs" $ \addr -> do
    execute2 0x4C addr 3
    pc' <- observe regPC
    assertEq "PC" pc' (pure addr)

jmp_ind :: Test
jmp_ind = op2 "JMP ind" $ \addr -> do
    addr' <- deref (pure addr)
    execute2 0x6C addr 5
    pc' <- observe regPC
    assertEq "PC" pc' addr'
