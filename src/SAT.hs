module SAT where

import Data.List
import qualified Data.Map.Strict as Map
import Types
import Eval
--import Data.IntMap.Strict (IntMap)
--import qualified Data.IntMap.Strict as IntMap

solver :: IO ()
solver = putStrLn "Not implemented yet"


{--Note about DIMACS form:
 * 'c' in the beginning denotes a comment
 * 'p' denotes the beginning of the problem description
 * 'cnf' refers to Conjunctive Normal Form: sets clauses of at most 3 literals
 * 'cnf V C' refers to V literals, C clauses, written as CNF
 * '0' is a terminating character, it means the end of a clause
 * Literals are deliminted by a space, clauses by a newline.
 below is an example of a CNF-3-2 formula: 3 literals, 2 clauses:
              "p cnf 3 2\n1 -3 0\n2 3 -1 0"
 and parsed to be in a [[String]] type:
 --}
toyFormula = [["p","cnf","3","2"],["1","-3","0"],["2","3","-1","0"]]








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

