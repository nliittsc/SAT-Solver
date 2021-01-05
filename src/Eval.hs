module Eval where

import Types
import qualified Data.Map as Map

-- takes a Map object representing a boolean assignment
-- to some variables, and a string representing a variable
-- and looks up the variables truth assignment (which may be
-- empty/unassigned)
getAssign :: TruthAssignment -> Id -> Maybe Bool
getAssign m x = Map.lookup (read x :: Int) m

-- a pattern matching function that recursively evaluates a
-- Boolean formula
eval :: TruthAssignment -> Formula -> Bool
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
