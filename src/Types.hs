module Types where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Set as Set

------------------------------------------------------
--  Types
------------------------------------------------------

-- a Map that represents a truth assignment to the variables
type TruthMap = IntMap Bool
type ClauseMap = IntMap Clause
type Clause = [Literal]
type Literal = Int
type CNF = ClauseMap
type Formula = [Clause]
--type Literal = Formula Not Var  -- example of a literal, probably  not needed


------------------------------------------------------
-- Data structures
------------------------------------------------------

-- keys: variables
-- values: keys to clauses that contain the variable


------------------------------------------------------
-- | Helper Functions
------------------------------------------------------

-- get all the variables out of a formula
getVars :: Formula -> [Int]
getVars f = Set.toList $ Set.unions sets
  where
    sets = fmap (Set.fromList . fmap abs) f

getTruth :: TruthMap -> Literal -> Bool
getTruth t x = f (IntMap.lookup k t)
  where
    k          = abs x :: Int
    f (Just b) = b
    f Nothing  = error "getTruth: No assignment to variable!"

evalLiteral :: TruthMap -> Literal -> Bool
evalLiteral t x
  | x < 0     = not $ getTruth t x
  | x > 0     = getTruth t x
  | otherwise = error "evalLiteral: Literal is zero!"

evalClause :: TruthMap -> Clause -> Bool
evalClause t c = or $ fmap (evalLiteral t) c

evalFormula :: TruthMap -> Formula -> Bool
evalFormula t formula = and $ fmap (evalClause t) formula

-- note: technically mapping over a IntMap object
evalCNF :: TruthMap -> CNF -> Bool
evalCNF t cnf = and (fmap (evalClause t) cnf)

