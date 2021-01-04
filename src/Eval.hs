{-- THIS FILE CONTAINS HELPER FUNCTIONS FOR EVALUATING A CNF CLAUSE --}

module Eval where

import Types
import qualified Data.Map as Map

-- takes a Map object representing a boolean assignment
-- to some variables, and a string representing a variable
-- and looks up the variables truth assignment (which may be
-- empty/unassigned)
getAssign :: TruthAssignment -> Id -> Maybe Bool
getAssign m x = f $ Map.lookup (read x :: Int) m
  where
    f (Just x) = x
    f Nothing = Nothing

-- a pattern matching function that recursively evaluates a
-- Boolean formula
eval :: TruthAssignment -> Formula -> Bool
eval t (Var (x, Nothing))   = getAssign t x == Just True
eval t (Not f)              = not (eval t f)
eval t (And f1 f2)          = and [eval t f1, eval t f2]
eval t (Or f1 f2)           = or [eval t f1, eval t f2]



{--
-- initializes all literals to True
initLiteral :: String -> Literal
initLiteral s = ("x" ++ s', True)
  where
    s' = if read s > 0 then s else tail s

-- construct a literal mapping
makeLiteralMap :: [[String]] -> LiteralMap
makeLiteralMap fmlaList = Map.fromList $ zip [1..numLiteral] (replicate numLiteral True)
  where
    numLiteral = read $ (last . init . head) fmlaList

makeClauseMap :: [[String]] -> ClauseMap
makeClauseMap fmlaList = Map.fromList $ zip [1..numClause] (map init $tail fmlaList)
  where
    numClause = read $ last . head $ fmlaList

-- ex: basically evaluates "-2" to "not x2", and "1" to "x1"
getLiteral :: String -> (Bool -> Bool, Int)
getLiteral s = (q, i)
  where
    s' = read s :: Int
    b = s' > 0
    q = if b then id else not
    i = if b then s' else -1 * s'

-- combined with a mapping, can actually evaluate a literal
evalLiteral :: LiteralMap -> String -> Bool
evalLiteral dict s = g (f $ Map.lookup i dict)
  where
    literal = getLiteral s
    g       = fst literal
    i       = snd literal
    f       = \(Just x) -> x

-- evaluates a CNF clause, but rquires looking up a literal
evalClause :: LiteralMap -> Clause -> Bool
evalClause dict = any (evalLiteral dict)

-- evaluates an entire CNF
evalCNF :: LiteralMap -> CNF -> Bool
evalCNF assignment = all (evalClause assignment)
--}