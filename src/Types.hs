module Types where

import Data.Map as Map ( Map )

type Literal = (String, Bool)
type Clause  = [String]
type LiteralMap = Map.Map Int Bool
type ClauseMap = Map.Map Int [String]
type CNF = [Clause]