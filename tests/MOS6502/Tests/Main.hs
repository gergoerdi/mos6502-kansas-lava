module MOS6502.Tests.Main (tests, foo) where

import Distribution.TestSuite.QuickCheck as QC
import Distribution.TestSuite

import MOS6502.Tests
import MOS6502.Tests.Framework
import qualified MOS6502.Tests.AllSuiteA as AllSuiteA
import qualified MOS6502.Tests.KlausDormann as KlausDormann

tests :: IO [QC.Test]
tests = do
    suiteTests <- sequence [ AllSuiteA.test, KlausDormann.test ]
    return $ suiteTests ++ opTests
  where
    opTests = [ testProperty (testLabel test) (runTest test)
              | test <- allTests
              ]

foo :: IO ()
foo = do
    Test t <- KlausDormann.test
    Finished r <- run t
    print r
