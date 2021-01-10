module DPLLSolver where

import Control.Monad.State
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Types

-- We will do this for readability first, optimize later
type SolverParams = Formula
type SolverOutPut = Bool

-- Currently only tests if satisfying or not
-- does not get an assignment
dpllSolver :: SolverParams -> IO SolverOutPut
dpllSolver formula = return $ evalState dpllSearch formula

dpllSearch :: State Formula Bool
dpllSearch = do
  f <- get
  if consistent f  -- Checks if the formula is satisfied
    then return True
    else do
      if hasEmpty f  -- If a clause is empty, we lose
        then return False 
        else do
          let f'  = unitPropogate f (getUnitClauses f)
          let f'' = pureLiteralElim f'
          if null f''
            then return True
            else do
              let l     = chooseLiteral f''
              let isSAT = evalState dpllSearch ([l] : f'')
              if isSAT
                then return isSAT
                else return $ evalState dpllSearch ([-l] : f'')
              
-----------------------------------------------------------------
-- Unit Propogation Functions
-----------------------------------------------------------------

-- gets all clauses that have length 1
getUnitClauses :: Formula -> [Clause]
getUnitClauses = filter (\xs -> length xs == 1)

unitPropogate :: Formula -> [Clause] -> Formula
unitPropogate f ls = evalState unitPropogate' (ls,f)

-- state monad will be used for updating truth assignments in later version
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

isPure :: Set Literal -> Int -> Bool
isPure lits x
  | Set.member x    lits && Set.notMember (-x) lits = True 
  | Set.member (-x) lits && Set.notMember x    lits = True
  | otherwise                                       = False


-- get all the variables out of a formula
getVars :: Formula -> [Int]
getVars f = Set.toList $ Set.unions sets
  where
    sets = fmap (Set.fromList . fmap abs) f

-- state monad will be used for updating truth assignments in later version
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
consistent :: Formula -> Bool
consistent f
  | onlySingletons f = evalState isConsistent' (lits, Set.toList lits)
  | otherwise = False
    where
      lits = getLiterals f


isConsistent' :: State (Set Literal,[Literal]) Bool
isConsistent' = do
  (litSet,litsToTest) <- get
  let r = uncons litsToTest
  if r == Nothing
    then return True
    else do
      let Just (l, rest) = r
      if Set.member l litSet && Set.member (-l) litSet
        then return False
        else do
          put (litSet,rest)
          isConsistent'


onlySingletons :: [[a]] -> Bool
onlySingletons xs = all (== 1) (fmap length xs)

hasEmpty :: Formula -> Bool
hasEmpty f = any null f && any (not . null) f

-- greedy: pick the literal that's in the most clauses
chooseLiteral :: Formula -> Literal
chooseLiteral f = fst $ getMax $ zip lits (fmap (countAppear f) lits)
  where
    lits = Set.toList $ getLiterals f

-- get literals in a formula
getLiterals :: Formula -> Set Literal
getLiterals f = Set.unions (fmap Set.fromList f)

getMax :: [(Int,Int)] -> (Int,Int)
getMax xs = last $ sortOn snd xs

countAppear :: Formula -> Literal -> Int
countAppear f l = sum $ fmap ((\x -> if x then 1 else 0) . (l `elem`)) f
  