module BruteSolver where

import Data.List
import qualified Data.IntMap.Lazy as IntMap
import Control.Monad.State
import Types

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

makeTruth :: Int -> [Int] -> TruthMap
makeTruth n bin = IntMap.fromAscList (zip [1..n] (map (==1) bin))

bruteSolver :: State (Int,Int,Int,CNF) (Bool,TruthMap)
bruteSolver = do
  (totalNum, currNum, numLit, formula) <- get
  let bin = padBin numLit $ toBin currNum
  let truth = makeTruth numLit bin
  let isSat = evalCNF truth formula
  if isSat || totalNum == currNum
    then return (isSat, truth)
    else do
      put (totalNum, currNum+1,numLit,formula)
      bruteSolver

  

