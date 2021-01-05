module BruteSolver where

import Data.List
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Types
import Eval

------------------------------------------------------------------------
-- | BruteForceSolver
------------------------------------------------------------------------

toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise      = toBin (n `div` 2) ++ [1]

padBin :: Int -> [Int] -> [Int]
padBin m xs = replicate (m - length ys) 0 ++ ys
    where ys = take m xs

makeTruth :: Int -> [Int] -> Truth
makeTruth n bin = Map.fromList (zip [1..n] (map (==1) bin))


-- >>> padBin 9 $ toBin (2^(10)-1)
-- [1,1,1,1,1,1,1,1,1]


bruteSolver :: State (Int,Int,Int,CNF) (Bool,Truth)
bruteSolver = do
  (totalNum, currNum, numLit, formula) <- get
  let bin = padBin numLit $ toBin currNum
  let truth = makeTruth numLit bin
  let isSat = eval truth formula
  if isSat || totalNum == currNum
    then return (isSat, truth)
    else do
      put (totalNum, currNum+1,numLit,formula)
      bruteSolver

  

