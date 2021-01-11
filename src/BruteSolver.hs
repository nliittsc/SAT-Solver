module BruteSolver where

import Data.List
import qualified Data.IntMap.Lazy as IntMap
import Control.Monad.State
import Types

------------------------------------------------------------------------
-- | BruteForceSolver
------------------------------------------------------------------------

toBin :: Integer -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise      = toBin (n `div` 2) ++ [1]

invBin :: [Int] -> Integer
invBin xs = sum $ convert (zip (map toInteger xs) (map toInteger $ reverse [0..n-1]))
  where
    n = length xs

convert :: [(Integer,Integer)] -> [Integer]
convert = map (\(i,m) -> i * 2^m)

padBin :: Int -> [Int] -> [Int]
padBin n xs = replicate (n - length ys) 0 ++ ys
    where ys = take n xs

makeTruth :: [Int] -> [Int] -> TruthMap
makeTruth vars bin = IntMap.fromAscList (zip vars (map (==1) bin))

-- top level brute solver
-- warning: Only use for a small number of variables!
bruteSolver :: Formula -> Bool
bruteSolver f = bruteSearch assignments f
  where
    vars = getVars f
    n = length vars
    bound = (2^n) -1 :: Integer
    assignments  = fmap (makeTruth vars . padBin n . toBin ) [0..bound]

bruteSearch :: [TruthMap] -> Formula -> Bool
bruteSearch [] f = False
bruteSearch (a:rest) f
  | evalFormula a f = True
  | otherwise       = bruteSearch rest f


  

