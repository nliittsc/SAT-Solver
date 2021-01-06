module Types where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

------------------------------------------------------
--  Types
------------------------------------------------------

-- a Map that represents a truth assignment to the variables
type TruthMap = IntMap Bool

-- a Map that allows access of a particular clause
type ClauseMap = IntMap Formula
type Id = String
type Variable = (Id, Maybe Bool)
type CNF = Formula
--type Literal = Formula Not Var  -- example of a literal, probably  not needed

{- | to reduce threading on stateful computations
SolverParams consists of:
  `maxTries :: Int` representing the stopping criterion
  `numLit :: Int` representing the number of literals
  `numClause :: Int` representing number of clauses
  `[Formula]` representing a list of clauses (for evaluation and sampling)
-}
type SolverParams = (Int,Int,Int,[Formula])

------------------------------------------------------
-- Data structures
------------------------------------------------------

-- recursive data type to represent a formula
data Formula = Var Variable
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             deriving (Eq, Show)

------------------------------------------------------
-- | Helper Functions
------------------------------------------------------

{- | These functions are used for formula evaluation
and parsing. It just seems more helpful to keep them all
in the same file. :)
-}


{- | takes a Map object representing a boolean assignment
to some variables, and a string representing a variable
and looks up the variables truth assignment (which may be
empty/unassigned)
-}
getAssign :: TruthMap -> Id -> Maybe Bool
getAssign mapping x = IntMap.lookup (read x :: Int) mapping

-- a pattern matching function that recursively evaluates a
-- Boolean formula
eval :: TruthMap -> Formula -> Bool
eval t (Var (x, Nothing))   = getAssign t x == Just True
eval t (Not f)              = not (eval t f)
eval t (And f1 f2)          = and [eval t f1, eval t f2]
eval t (Or f1 f2)           = or [eval t f1, eval t f2]

clauseToList :: Formula -> [Formula]
clauseToList (f1 `Or` f2 `Or` f3) = [f1, f2, f3]
clauseToList (f1 `Or` f2)         = [f1, f2]
clauseToList f                    = [f]

cnfToList :: Formula -> [Formula]
cnfToList (c1 `And` c2) = c1 : cnfToList c2
cnfToList formula       = [formula]
