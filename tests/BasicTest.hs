module Main where
import Hasktan
import Test.GHUT
import Test.HUnit
import qualified System.Exit as Exit

test1 :: Test
test1 = TestCase (assertEqual "37" 37 (interpret "30 + 7"))
 
tests :: Test
tests = TestList [TestLabel "test1" test1]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
 
 
