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

type TruthAssignment = Map.Map Int (Maybe Bool)
type Id = String
type Variable = (Id, Maybe Bool)
type CNF = Formula
--type Literal = Formula Not Var  -- example of a literal, probably  not needed

------------------------------------------------------
-- Data structures
------------------------------------------------------


-- recursive data type to represent a formula
data Formula = Var Variable
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             deriving (Eq, Show)


