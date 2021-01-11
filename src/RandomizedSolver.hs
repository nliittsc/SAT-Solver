module RandomizedSolver where

import Control.Monad.State
import qualified Data.IntMap.Lazy as IntMap
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random
import Types

------------------------------------------------------------------------
-- | Randomized 3-Sat Algorithm
------------------------------------------------------------------------

type SolverParams = (Int, Int, Formula)
type Assignment   = TruthMap
type SolverOutput = (Bool,Assignment)

-- top level solver
monteCarloSolver :: SolverParams -> IO SolverOutput
monteCarloSolver (maxTries, numVar, formula)
  = do
    g <- getStdGen
    let bound = 2^numVar - 1 :: Integer
    let possibleTruths = randomRs (0, bound) g
    let nums = take maxTries possibleTruths
    let attempts = toTruths numVar nums
    g' <- newStdGen
    let startState = (g',numVar,attempts,Set.fromList formula)
    let (isSAT,truth) = evalState randomWalk startState
    return (isSAT,truth)

type Assignments = [Assignment]
type SolverState = (StdGen,Int,Assignments, Set Clause)

-- Random Walk Algorithm for 3-SAT
randomWalk :: State SolverState (Bool,Assignment)
randomWalk = do
  (g,numVar,rndTruths,clauses) <- get
  let r = uncons rndTruths
  if r == Nothing  -- no assignments to try, algorithm terminates
    then return (False,IntMap.empty)
    else do
      -- begin stochastic local search
      let Just (truth,nextTruths) = r
      let numSteps    = 3 * numVar
      let startStep   = 0
      let searchState = (g,numSteps,startStep,truth,clauses)
      let (isSAT,truth',g') = evalState localSearch searchState
      if isSAT
        then return (isSAT,truth')
        else do
          put (g',numVar,nextTruths,clauses)
          randomWalk

type SearchState = (StdGen,Int,Int,Assignment,Set Clause)
type SearchOut   = (Bool,Assignment,StdGen)


-- the `stochastic local search` step
localSearch :: State SearchState SearchOut
localSearch = do
  (g,n,k,truth,clauses) <- get
  let isSAT = all (evalClause truth) clauses
  if isSAT || k > n
    then return (isSAT,truth,g)
    else do
      let (g',g'')       = split g
      let (g''',g'''')   = split g'
      let unsatClauses   = getBroken truth clauses
      let (_,badClause) = randomChoice unsatClauses g''
      let (_,var)      = randomChoice (Set.fromList badClause) g'''
      let truth'         = modifyTruth var truth
      put (g'''',n,k+1,truth',clauses)
      localSearch

-- get UNSAT clauses from list of clauses (formula)
getBroken :: Assignment -> Set Clause -> Set Clause
getBroken truth = Set.filter (not . evalClause truth)

-- helper to randomly get an object from list
-- Not optimized :(
randomChoice :: Set a -> StdGen -> (StdGen,a)
randomChoice set g = (g',x)
  where
    n      = Set.size set
    (i,g') = uniformR (0,n-1) g
    x      = Set.elemAt i set

modifyTruth :: Literal -> Assignment -> Assignment
modifyTruth literal = IntMap.alter flipBit x
  where
    x = abs literal :: Int

flipBit :: Maybe Bool -> Maybe Bool
flipBit (Just bool) = Just (not bool)
flipBit Nothing     = Nothing

toBin :: Integer -> [Integer]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise      = toBin (n `div` 2) ++ [1]

padBin :: Int -> [Integer] -> [Integer]
padBin m xs = replicate (m - length ys) 0 ++ ys
    where ys = take m xs

makeTruth :: Int -> [Integer] -> TruthMap
makeTruth n bin = IntMap.fromAscList (zip [1..n] (map (==1) bin))

toTruths :: Int -> [Integer] -> [TruthMap]
toTruths n attempts = fmap (makeTruth n) binaries
  where
    binaries = fmap (padBin n . toBin) attempts

  
