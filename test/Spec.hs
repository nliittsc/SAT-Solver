{-

import System.Directory (listDirectory)
import BruteSolver ( bruteSolver )
import RandomizedSolver ( random3SAT )
import ParseCNF
import Types
import Data.Time ( diffUTCTime, getCurrentTime )
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
        let (numLit,numClause,cnfFormula) = goodResult
        initGen <- getStdGen
        let maxTries = 100
        let clauseList = cnfToList cnfFormula
        let solverParams = (maxTries,numLit,numClause,clauseList)
        let initState = (0,initGen,solverParams)
        let (isSat,truth) = evalState random3SAT initState
        if isSat
          then do
            print path
            print "SATISFIABLE"
            return (isSat,path)
          else do
            print path
            print "UNSATISFIABLE"
            return (isSat,path)



count = foldl (\i v -> if v then i + 1 else i) 0

main :: IO ()
main = do
  start <- getCurrentTime
  let folderPath = "test\\test-formulas\\testcases" :: FilePath
  contents <- listDirectory folderPath
  let contents' = reverse contents
  let contents'' = take 6 contents'
  let folderPath' = folderPath ++ "\\" :: FilePath
  results <- mapM (rndSolve . (folderPath' ++)) contents''
  let numSat = count (map fst results)
  print "Number of cases:"
  print $ length results
  print "Number of SAT: "
  print numSat
  stop <- getCurrentTime
  print $ diffUTCTime stop start
-}