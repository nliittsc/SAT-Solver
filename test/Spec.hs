import System.Directory (getDirectoryContents)
import SAT
import Data.List
import ParseCNF
import qualified Data.Map as Map
import Data.Time
import Text.Parsec.String




main :: IO ()
main = do
  assign <- randomAssignment 3
  print assign
  --f <- readFile "test\\test-formulas\\satisfiable\\uf20-01.cnf"
  {--print f
  result <- parseFromFile cnfFileP "test\\test-formulas\\smallcases\\f0010-02-s.cnf"
  case result of
       Left err -> print err
       Right (assignment, formula) -> do {print assignment; print formula}
        --}









  {--
  --l <- readFile "test\\test-formulas\\simple.cnf"
  --print l
  start <- getCurrentTime
  paths' <- getDirectoryContents "test\\test-formulas\\smallcases"
  let paths = (init . init) paths'
  print paths
  let file = head paths
  let path = "test\\test-formulas\\smallcases\\" ++ file
  let testCase = last $ splitBy '\\' path
  (numLiteral, numClauses, cnf) <- getCNF path
  let (isSatis, assignment) = bruteSolver numLiteral cnf
  print $ "Testing " ++ testCase 
  if isSatis
    then do
      print "SATISFIABLE"
      print assignment
    else
      print "UNSATISFIABLE"
  stop <- getCurrentTime
  print $ diffUTCTime stop start
--}

