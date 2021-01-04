{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.Map as Map ( Map )
import qualified Data.Map (lookup)
import Prelude hiding (lookup)

type Literal = (String, Bool)
type Clause  = [String]
type LiteralMap = Map.Map Int Bool
type Assignment = LiteralMap
type TruthAssignment = Map.Map Int (Maybe Bool)
type ClauseMap = Map.Map Int [String]
type CNF = [Clause]


type Id = String
type Variable = (Id, Maybe Bool)
-- recursive data type to represent a formula
data Formula = Var Variable
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             deriving (Eq, Show)

--type CNF = Formula
--type Literal = Formula Not Var  -- example of a literal, probably  not needed

eval :: Formula -> Bool
eval (Var (x, Nothing))   = error "Literal not assigned"
eval (Var (x, Just bool)) = bool
eval (Not f)              = not (eval f)
eval (And f1 f2)          = eval f1 && eval f2
eval (Or f1 f2)           = eval f1 || eval f2