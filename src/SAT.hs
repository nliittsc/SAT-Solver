module SAT where

import Data.List
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Types
import Eval
import Control.Monad (replicateM)
import Control.Monad.Random

-- ad-hoc solution to generate random assignments

bernoulli :: RandomGen g => Rand g [Bool]
bernoulli = getRandomRs (False, True)

outcomes :: RandomGen g => Rand g [Bool]
outcomes = do bernoulli

gen = mkStdGen 193204829530

g = next gen

-- >>> next gen
-- (2016461655,2016502347 40692)


f = evalRand outcomes gen

-- ProgressCancelledException
-- >>> take 3 $ evalRand outcomes gen
-- [False,False,False]


randomAssignment n = do
  gen <- getStdGen
  let values = take n $evalRand outcomes gen
  let assign = zip [1..n] values
  return assign


-- TODO: FIGURE OUT PSEUDO RANDOMNESS

-- implementation of the walkSAT algorithm
solverWalkSAT :: Int -> CNF ->
                 State (Int, StdGen, TruthAssignment) (Int, StdGen, TruthAssignment)

solverWalkSAT maxTries cnf = do
  s <- get
  let k = (\(x, _, _) -> x) s
  let gen = (\(_, x, _) -> x) s
  let truth = (\(_, _, x) -> x) s
  let result = eval truth cnf
  if result || k == maxTries
    then return s
    else do
      let n = length $ Map.elems truth
      let gen' = next gen
      --let newRandomAssign = take n $ 
      return s





 
toyFormula = [["p","cnf","3","2"],["1","-3","0"],["2","3","-1","0"]]

{--
-- pass how many literals there are, the clause to be satifisfied
bruteSolver :: Int -> CNF -> (Bool, Assignment)
bruteSolver n cnf = ((not.null) assignment, assignment)
  where
    assignment = search cnf (genAllBools n)

search :: CNF -> [[(Int, Bool)]] -> Assignment
search cnf [] = Map.empty 
search cnf (x:xs)
  | evalCNF assignment cnf = assignment
  | otherwise              = search cnf xs
    where
      assignment = Map.fromList x

genAllBools :: Int -> [[(Int, Bool)]]
genAllBools n = mapM (\v -> [(v,True),(v,False)]) [1..n]

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]] 
            where f c l@(x:xs) | c == delimiter = []:l
                               | otherwise = (c:x):xs
-- >>> splitBy '\\' "okay\\yes"
-- ["okay","yes"]

-- >>> 2^100
-- 1267650600228229401496703205376




-- >>> evalClause (makeLiteralMap toyFormula) ["2", "3", "-1"]
-- True

--makeCNFMap :: [[String]] -> Map.Map Int Clause
--makeCNFMap stringFmla = Map.fromList $ zip [1..numClause] (map f $ tail stringFmla)
--  where
--    numClause = read $ last $ head stringFmla :: Int
--    f xs = map getLiteral (init xs)

-- >>> makeCNFMap toyFormula
-- fromList [(1,[("x1",True),("x3",False)]),(2,[("x2",True),("x3",True),("x1",False)])]


-- >>> or [True]
-- True

--}