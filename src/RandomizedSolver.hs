module RandomizedSolver where

import Control.Monad.State
import Control.Monad.Random
import qualified Data.Map as Map
import Data.List
import Types
import Eval

------------------------------------------------------------------------
-- | Randomized 3-Sat Algorithm
------------------------------------------------------------------------

-- gets a new random assignment of truth values
randomAssign :: Int -> StdGen -> ([Bool], StdGen)
randomAssign 0 gen = ([], gen)
randomAssign n gen = (bool : restOfList, gen'')
  where
    (bool, gen') = random gen :: (Bool, StdGen)
    (restOfList, gen'') = randomAssign (n-1) gen'

-- a funcion to randomly select an element from a list of elements
randomChoice :: [a] -> StdGen -> (a, StdGen)
randomChoice [] g = error "passed list is empty"
randomChoice ls g = (v, g')
  where
    n = length ls
    (i, g') = randomR (0, n-1) g
    v = ls !! i

-- assign new truth assignment
makeNewTruth :: Int -> [Bool] -> Truth
makeNewTruth n bools = Map.fromList (zip [1..n] bools)

-- helper to randomly select an unsatisfied clause
getBadClause :: TruthAssignment -> [Formula] -> StdGen -> (Formula, StdGen)
getBadClause truth cnf g = (badFmla, g')
  where
    (badFmla, g') = randomChoice (filter (not . eval truth) cnf) g :: (Formula, StdGen)

-- helper to randomly select an literal from the bad clause
pickVar :: TruthAssignment -> Formula -> StdGen -> (Formula, StdGen)
pickVar truth clause = randomChoice (clauseToList clause)

-- helper to flip a boolean in a Mapping
flipBit :: Maybe Bool -> Maybe Bool
flipBit (Just bool) = Just (not bool)
flipBit Nothing     = Nothing

-- helper that consumes a literal and flips the bit
modifyAssignment :: TruthAssignment -> Formula -> TruthAssignment
modifyAssignment truth (Not (Var (x, _))) = Map.alter flipBit (read x) truth
modifyAssignment truth (Var (x, _)) = Map.alter flipBit (read x) truth
modifyAssignment truth _            = error "Did not pass a Var"

{- | This function implements the "local search" part of the stochastic local search
    The first parameter of the `State` has type (Int, StdGen, and Truth). The first
    `Int` is a counter, modeling how many steps have been taken (and must terminate
    when the Int `maxSteps` is reached. The second argument, `StdGen` is a generator
    that is used in order to do random sampling. The final argument `Truth` represents
    the current truth assignment to the variables in the Boolean Formula.
    
    Finally, the second parameter of the `State` is output of the walking function.
    The first argument `Bool` indicates whether a satisfying assignment was found
    or not. The second `StdGen` indicates the current generator (to be threaded to
    the solver, if needed for another round. The last argument is an assignment,
    just in case.
-}
randomWalk :: Int -> [Formula] -> State (Int,StdGen,Truth) (Bool,StdGen,Truth)
randomWalk maxSteps cnf = do
  (k,gen,currTruth) <- get
  let isSat = all (eval currTruth) cnf
  if isSat || k == maxSteps
    then return (isSat,gen,currTruth)
    else do
      let (badClause, gen') = getBadClause currTruth cnf gen
      let (literal, gen'')  = pickVar currTruth badClause gen'
      let newTruth          =  modifyAssignment currTruth literal
      put (k+1, gen'', newTruth)
      randomWalk maxSteps cnf

{- | 'Top-level' stateful solver. This implements a 'naive' randomized 3SAT algorithm.
      On every step, the current state is read in, a random Truth assignment
      is generated, and then the stateful `randomWalk` computation is executed
      obtaining a result. If `isSat` == True, then the state output is this Bool
      and the satisfying assignment. Otherwise, update the state and try again.

      If `maxTries` is reached, then the algorithm terminates, and returns a False
      indicated that the formula is unsatisfiable (with high probability).
 -}
random3SAT :: State (Int,StdGen,SolverParams) (Bool,Truth)
random3SAT = do
  (k,gen,params) <- get
  let (maxTries,numLit,numClause,clauseList) = params
  let (bools, gen') = randomAssign numClause gen
  let rndTruth = makeNewTruth numLit bools
  let maxSteps = 3 * numLit :: Int
  let startState = (0,gen',rndTruth)
  let (isSat,gen'',truth) = evalState (randomWalk maxSteps clauseList) startState
  if isSat || k == maxTries
    then return (isSat,truth)
    else do
      put (k+1,gen'',params)
      random3SAT


{- | Helper funciton to calculate how many tries one should do.
The expected number of steps to solve the problem is O(n^(3/2) * (4/3)^n).
Since on each 'round', 3*n steps are executed, the number of rounds
one should try should be roughly (n^(3/2) * (4/3)^n) / (3*n).
WARNING: This function grows extremely fast! Not recommended.
E.g.: n=10, b=2 results in 74 tries but
      n=50, b=2 results in 16647942 tries
      n=100, b=2 results in 41573098802772 tries
-}
getMaxTries :: Int -> Int -> Int
getMaxTries n b = m
  where
    n' = fromIntegral n
    a = ceiling $ (n' ** (3/2)) * ((4/3) ** n')
    m = (2 * a * b) `div` (3 * n)


-- >>> 1 + 2