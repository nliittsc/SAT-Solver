module MyTests where

import DPLLSolver
import BruteSolver
import Data.List
import Types
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Control.Monad.State


--------------------------------------------------------
-- | Helpers
--------------------------------------------------------


--------------------------------------------------------
-- | Generate Uniform 3-SAT
--------------------------------------------------------

genLiteral :: Int -> Gen Int
genLiteral n = oneof [negLits, posLits]
  where
    negLits = choose (-n,-1) :: Gen Int
    posLits = choose (1,n) :: Gen Int


genClause :: Int -> Gen [Int]
genClause n = vectorOf 3 (genLiteral n)
    `suchThat`
    (not . repeated)
    `suchThat`
    (not . hasNegation)

repeated :: [Int] -> Bool
repeated [] = False
repeated (x:xs)
  | x `elem` xs = True
  | otherwise   = repeated xs

hasNegation :: [Literal] -> Bool
hasNegation [] = False
hasNegation (x:xs)
  | (-x) `elem` xs = True
  | otherwise      = hasNegation xs


genThreeSAT :: Int -> Int -> Gen Formula
genThreeSAT n k = vectorOf k (genClause n)


-- TODO: Figure out why this randomly will hang
arbClause :: Gen Clause
arbClause = sized $ \n ->
  do k <- choose (3,n)
     genClause k


--------------------------------------------------------
-- | Properties
--------------------------------------------------------

-- tests that we can convert to and from binary numbers
prop_bin_conv :: Int -> Bool
prop_bin_conv n = n' == invBin (toBin n')
  where
    n' = toInteger n

-- tests that padding works correctly
prop_bin_count :: Int -> Bool
prop_bin_count n = (bound+1) == len
  where
    n' = toInteger n
    bound = 2^n'-1 :: Integer
    nums = [0..bound]
    allBins = fmap toBin nums
    len = toInteger $ length allBins

-- no clause can have a repeated literal, or be a tautology, or contain 0
prop_valid_clause :: Clause -> Bool
prop_valid_clause c = (not . repeated) c && (not . hasNegation) c && 0 `notElem` c

-- compares the DPLL algorithm against a brute force
prop_dpll_correct :: Formula -> Bool
prop_dpll_correct f = bruteAnswer == dpllAnswer
  where
    bruteAnswer = bruteSolver f
    dpllAnswer  = evalState dpllSearch f

runMyTests :: IO ()
runMyTests = do
  print "Checking positive Int generation..."
  quickCheck $ forAll (choose (0,1000) :: Gen Int) (\x -> x >= 0 && x <= 1000)
  print "Checking binary conversion..."
  quickCheck $ forAll (choose (0,1000) :: Gen Int) prop_bin_conv
  print "Checking that all assignments are generated..."
  quickCheck $ forAll (choose (0,20) :: Gen Int) prop_bin_count 
  print "Checking if clauses are valid..."
  quickCheck $ forAll (genClause 1000) prop_valid_clause
  print "Testing DPLL Solver..."
  quickCheck $ forAll (genThreeSAT 20 50) prop_dpll_correct
