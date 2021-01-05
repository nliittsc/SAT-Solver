{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.Map as Map ( Map )
import qualified Data.Map (lookup)
import Prelude hiding (lookup)

------------------------------------------------------
--  Types
------------------------------------------------------

type TruthAssignment = Map.Map Int Bool
type Truth = TruthAssignment    -- saves some typing
type Id = String
type Variable = (Id, Maybe Bool)
type CNF = Formula
--type Literal = Formula Not Var  -- example of a literal, probably  not needed

{- | to reduce threading on stateful computations
SolverParams consists of:
  `maxTries :: Int` representing the stopping criterion
  `numLit :: Int` representing the number of literals
  `numClause :: Int` representing number of clauses
  `[Formula]` representing a list of clauses (for evaluation and sampling)
-}
type SolverParams = (Int,Int,Int,[Formula])


------------------------------------------------------
-- Data structures
------------------------------------------------------



-- recursive data type to represent a formula
data Formula = Var Variable
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             deriving (Eq, Show)


