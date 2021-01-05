module Main where

import System.Environment
import Types
import Eval
import ParseCNF
import SAT  
import Text.Parsec.String
import Control.Monad.Random

main :: IO ()
main = putStrLn "Not implemented yet"
{-
main = do
  [p] <- getArgs    -- get the path
  result <- parseFromFile cnfFileP p
  case result of
       Left _  -> print "Something went wrong during parsing"
       Right x -> do
        let (numLiteral, numClause, cnf) = x   -- get parameters from parsed .cnf
        g <- getStdGen
        let (maxTries, gen) = randomR (10,20) g :: (Int, StdGen)  -- placeholder
        print "OK"
-}
