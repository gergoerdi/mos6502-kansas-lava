import MOS6502.Tests
import MOS6502.Tests.Framework
import Test.QuickCheck
import Control.Monad (forM_)

main :: IO ()
main = do
    forM_ allTests $ \test -> do
        putStrLn $ testLabel test
        quickCheck $ runTest test
