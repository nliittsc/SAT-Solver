import System.Directory (listDirectory)
import BruteSolver ( bruteSolver )
import RandomizedSolver ( monteCarloSolver )
import DPLLSolver
import ParseCNF
import Types
import Data.Time ( diffUTCTime, getCurrentTime )
import Text.Parsec.String ( parseFromFile )
import System.Random
import Control.Monad.State ( evalState )
--import Data.IntMap.Lazy as IntMap


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
            print path
            print "SATISFIED"
            return (isSat,path)
          else do
            print path
            print "UNSATISFIED"
            return (isSat,path)


rndSolve :: FilePath -> IO (Bool,FilePath)
rndSolve path = do
  result <- parseFromFile cnfFileP path
  case result of
      Left err -> do {print err; return (False,path)}
      Right goodResult -> do
        let (numVar,numClause,cnfFormula) = goodResult
        let maxTries = 100
        --print path
        output <- monteCarloSolver (maxTries,numVar,cnfFormula)
        let (isSAT,assignment) = output
        if isSAT
          then do
            print path
            print "was SAT"
            return (isSAT,path)
          else do
            print path
            print "was UNSAT"
            return (isSAT,path)



count :: [Bool] -> Integer
count = foldl (\i v -> if v then i + 1 else i) 0

determSolve :: FilePath -> IO (Bool, FilePath)
determSolve path = do
  result <- parseFromFile cnfFileP path
  case result of
    Left err -> do {print err; return (False,path)}
    Right goodResult -> do
      let (numLit,numClause,cnfFormula) = goodResult
      output <- dpllSolver cnfFormula
      let isSAT = output
      if isSAT
        then do
          print path
          print "was SAT"
          return (True,path)
        else do
          print path
          print "was UNSAT"
          return (False,path)


main :: IO ()
main = do
  start <- getCurrentTime
  let folderPath = "test\\test-formulas\\testcases\\sat-uniform" :: FilePath
  contents <- listDirectory folderPath
  let contents' = reverse contents
  --let contents'' = take 6 contents'
  let folderPath' = folderPath ++ "\\" :: FilePath
  results <- mapM (rndSolve . (folderPath' ++)) contents'
  let numSat = count (map fst results)
  print "Number of cases:"
  print $ length results
  print "Number of SAT: "
  print numSat
  stop <- getCurrentTime
  print $ diffUTCTime stop start
