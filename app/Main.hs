module Main where

import System.Environment
import ParseCNF
import DPLLSolver

main :: IO ()
main = do
  fs <- getArgs
  case fs of
      [f] -> solveFile f
      _ -> error "Invalid argument!"

solveFile :: FilePath -> IO ()
solveFile path = do
  contents <- readFile path
  let result = parseFromString cnfFileP contents
  case result of
      Left err -> print err
      Right goodResult -> do
        let (numVar,numClause,formula) = goodResult
        putStrLn $ "Solving problem in <" ++ path ++ ">"
        sat <- dpllSolver formula
        if sat
          then putStrLn "SAT"
          else putStrLn "UNSAT"