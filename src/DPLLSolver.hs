module DPLLSolver where

import Control.Monad.State
import qualified Data.IntMap.Lazy as IntMap
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Types
import System.Random

-- We will do this for readability first, optimize later
type Assignment = IntMap.IntMap (Maybe Bool)
type SolverParams = (Int,Int,Formula)
--type SolverOutPut = (Bool,Assignment)
type SolverOutPut = Bool


-- Currently only tests if satisfying or not
-- does not get an assignment
dpllSolver :: SolverParams -> IO SolverOutPut
dpllSolver (numVar,numClause,formula) = do
  --let initTruth = IntMap.fromList (zip [1..numVar] (replicate numVar Nothing))
  let literals = getLiterals formula
  let startState = (numVar,literals,formula)
  return $ evalState dpllSearch startState

dpllSearch :: State (Int,[Int],Formula) Bool
dpllSearch = do
  (numVar,literals,formula) <- get
  -- check consistency, if consistent we win
  if isConsistent formula
    then return True
    else do
      -- check for an empty clause, if empty we lose
      if hasEmpty formula
        then return False
        else do
          -- proceed with back-tracking search
          let unitClauses = getUnitClauses formula
          let formula' = unitPropogate formula unitClauses
          let formula'' = pureLiteralElim formula'
          if null formula''
            then return True
            else do
              let l = chooseLiteral formula'' -- should be unassigned literal
              let isSat1 = evalState dpllSearch (numVar,literals, [l] : formula'')
              if isSat1
                then return isSat1
                else do
                  let formula0 = subst formula l "neg"  -- replace all occurrences of l with 'false'
                  let isSat0 = evalState dpllSearch (numVar,literals,[-l] : formula'')
                  return isSat0
              
-----------------------------------------------------------------
-- Unit Propogation Functions
-----------------------------------------------------------------

-- gets all clauses that have length 1
getUnitClauses :: Formula -> [Clause]
getUnitClauses = filter (\xs -> length xs == 1)

unitPropogate :: Formula -> [Clause] -> Formula
unitPropogate f ls = evalState unitPropogate' (ls,f)

unitPropogate' :: State ([Clause], Formula) Formula
unitPropogate' = do
  (ls,f) <- get
  let r = uncons ls
  if r == Nothing
    then return f
    else do
      let Just ([l],clauses) = r
      -- TODO: Figure out truth assignment updates
      let f' = delClauses [l] f
      let f'' = delLiteral l f'
      put (clauses,f'')
      unitPropogate'

-- remove every clause containing the literal l
-- neccesary to add backc in the unit clause
delClauses :: Clause -> Formula -> Formula
delClauses [l] f = [l] : filter (l `notElem`) f

-- delete -l from every clause
delLiteral :: Literal -> Formula -> Formula
delLiteral l = fmap (delete (-l))

-----------------------------------------------------------------
-- Pure Literal Elimination
-----------------------------------------------------------------

pureLiteralElim :: Formula -> Formula
pureLiteralElim f = evalState pureLitElim (pureVars,f)
  where
    pureVars = getPureVars f

getPureVars :: Formula -> [Int]
getPureVars f = filter (isPure lits) vars
  where
    lits = getLiterals f
    vars = getVars f

isPure :: [Literal] -> Int -> Bool
isPure lits x
  | x `elem` lits && (-x) `notElem` lits = True 
  | (-x) `elem` lits && x `notElem` lits = True
  | otherwise                            = False


-- get all the variables out of a formula
getVars :: Formula -> [Int]
getVars f = Set.toList $ Set.unions sets
  where
    sets = fmap (Set.fromList . fmap abs) f

pureLitElim :: State ([Int],Formula) Formula
pureLitElim = do
  (ls,f) <- get
  let r = uncons ls
  if r == Nothing
    then return f
    else do
      let Just (x,restOfPureVars) = r
      let f' = delPure x f
      put (restOfPureVars,f')
      pureLitElim

-- removes any clauses which contain the pure variable
delPure :: Int -> Formula -> Formula
delPure x f = filter ((-x) `notElem`) $ filter (x `notElem`) f


-----------------------------------------------------------------
-- Helper Functions
-----------------------------------------------------------------

-- checks if the formula is "consistent"
isConsistent :: Formula -> Bool
isConsistent f
  | null f = True
  | onlySingletons f = evalState isConsistent' (Set.fromList lits, lits)
  | otherwise = False
    where
      lits = getLiterals f

onlySingletons :: [[a]] -> Bool
onlySingletons xs = all (== 1) (fmap length xs)

isConsistent' :: State (Set Literal,[Literal]) Bool
isConsistent' = do
  (litSet,litsToTest) <- get
  let r = uncons litsToTest
  if r == Nothing
    then return True
    else do
      let Just (lit, rest) = r
      if Set.member lit litSet && Set.member (-lit) litSet
        then return False
        else do
          put (litSet,rest)
          isConsistent'


hasEmpty :: Formula -> Bool
hasEmpty f = any null f && any (not . null) f

-- greedy: pick the literal that's in the most clauses
chooseLiteral :: Formula -> Literal
chooseLiteral f = fst $ getMax $ zip lits (fmap (countAppear f) lits)
  where
    lits = getLiterals f

-- get literals in a formula
getLiterals :: Formula -> [Literal]
getLiterals f = Set.toList $ Set.unions (fmap Set.fromList f)

getMax :: [(Int,Int)] -> (Int,Int)
getMax xs = last $ sortOn snd xs

countAppear :: Formula -> Literal -> Int
countAppear f l = sum $ fmap ((\x -> if x then 1 else 0) . (l `elem`)) f

subst :: Formula -> Literal -> String -> Formula
subst f l s = case s of
  "pos" -> posReplace f l
  "neg" -> negReplace f l

-- replaces all pos/neg literals with a "true" literal
posReplace :: Formula -> Literal -> Formula
posReplace f l = map (replace l) f

-- replaces all pos/neg literals with a "false" literal
negReplace :: Formula -> Literal -> Formula
negReplace f l = map (replace (-l)) f

-- replaces all a or -a with a
replace :: Int -> [Int] -> [Int]
replace a [] = []
replace a (x:xs)
  | a == x    = a : replace a xs
  | a == -x   = a : replace a xs
  | otherwise = x : replace a xs


  