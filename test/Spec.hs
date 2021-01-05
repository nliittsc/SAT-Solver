import System.Directory (listDirectory)

import BruteSolver ( bruteSolver )
import RandomizedSolver ( random3SAT )
import Eval
import Data.List
import ParseCNF
import qualified Data.Map as Map
import Data.Time
import Text.Parsec.String ( parseFromFile )
import Control.Monad.Random ( getStdGen )
import Control.Monad.State ( evalState )


bruteSolve :: FilePath -> IO (Bool,FilePath)
bruteSolve path = do
  result <- parseFromFile cnfFileP path
  case result of
      Left err -> do {print err; return (False,path)}
      Right goodResult -> do
        let (numLit,numClause,cnfFormula) = goodResult
        let totalNum = 2^numLit :: Int
        let initState = (totalNum,0,numLit,cnfFormula)
        let (isSat,truth) = evalState bruteSolver initState
        if isSat
          then do
            print "SATISFIED"
            print path
            return (isSat,path)
          else do
            print "UNSATISFIED"
            print path
            return (isSat,path)
            --print "No assignment found!"


{- | helper to get the maximum number of tries
upper bounds the probability that no assignment is found by (1/2)^b
when a satisfying assignment exists
-}

getMaxTries :: Int -> Int -> Int
getMaxTries n b = 2 * a * b
  where
    n' = fromIntegral n
    a = ceiling $ (n' ** (3/2)) * ((4/3) ** n')


rndSolve :: FilePath -> IO (Bool,FilePath)
rndSolve path = do
  result <- parseFromFile cnfFileP path
  case result of
      Left err -> do {print err; return (False,path)}
      Right goodResult -> do
        let (numLit,numClause,cnfFormula) = goodResult
        initGen <- getStdGen
        --let maxTries = getMaxTries numLit 1
        --print maxTries
        let maxTries = 5000
        let clauseList = cnfToList cnfFormula
        let solverParams = (maxTries,numLit,numClause,clauseList)
        let initState = (0,initGen,solverParams)
        let (isSat,truth) = evalState random3SAT initState
        if isSat
          then do
            print "SATISFIABLE"
            print path
            return (isSat,path)
          else do
            print "UNSATISFIABLE"
            print path
            return (isSat,path)

count = foldl (\i v -> if v then i + 1 else i) 0




main :: IO ()
main = do
  start <- getCurrentTime
  let folderPath = "test\\test-formulas\\smallAIM" :: FilePath
  contents' <- listDirectory folderPath
  let contents = take 5 contents'
  let folderPath' = folderPath ++ "\\" :: FilePath
  results <- mapM (rndSolve . (folderPath' ++)) contents
  let numSat = count (map fst results)
  print "Number of cases:"
  print $ length results
  print "Number of SAT: "
  print numSat
  stop <- getCurrentTime
  print $ diffUTCTime stop start