module SAT where

import Data.List
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Types
import Eval
--import Data.IntMap.Strict (IntMap)
--import qualified Data.IntMap.Strict as IntMap

bruteForceSolver :: IO ()
bruteForceSolver = putStrLn "Not implemented yet"

 
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