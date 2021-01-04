{-- THIS FILE CONTAINS HELPER FUNCTIONS FOR EVALUATING A CNF CLAUSE --}

module Eval where

import Types
import qualified Data.Map as Map


eval :: Formula -> Bool
eval (Var (x, Nothing))   = error "Literal not assigned"
eval (Var (x, Just bool)) = bool
eval (Not f)              = not (eval f)
eval (And f1 f2)          = eval f1 && eval f2
eval (Or f1 f2)           = eval f1 || eval f2



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