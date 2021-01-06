module ParseCNF where
import Types
import Text.ParserCombinators.Parsec
import Data.List


intP :: Parser Int
intP = do
  n <- many1 digit
  return (read n)

negIntP :: Parser Int
negIntP = do
  _ <- char '-'
  x <- intP
  return (-x)

literalP :: Parser Literal
literalP = try negIntP <|> intP

-- parses a clause with 3 variables (also consums terminating character)
threeLitP = do
  x <- literalP
  _ <- space
  y <- literalP
  _ <- space
  z <- literalP
  _ <- space
  _ <- char '0'
  return [x, y, z]

twoLitP = do
  x <- literalP
  _ <- space
  y <- literalP
  _ <- space
  _ <- char '0'
  return [x, y]

oneLitP = do
  x <- literalP
  _ <- space
  _ <- char '0'
  return [x]

-- parses literals seperated by a space as an 'Or' formula
clauseP :: Parser Clause
clauseP = try threeLitP <|> try twoLitP <|> oneLitP

-- recursively parses a CNF formula
baseCNFP :: Parser Formula
baseCNFP = do
  c <- clauseP
  return [c]

twoCNFP :: Parser Formula
twoCNFP = do
  c1 <- clauseP
  _  <- space
  c2 <- clauseP
  return [c1, c2]  

recursiveCNFP :: Parser Formula
recursiveCNFP = do
  c1 <- clauseP
  _  <- space
  c2 <- try recursiveCNFP <|> try twoCNFP <|> baseCNFP 
  return (c1 : c2)

-- top level parser: either recursively solves parses or just returns a single clause
cnfP :: Parser Formula
cnfP = try recursiveCNFP <|> baseCNFP

-- to skip a comment
comment :: Parser String
comment = do
  char 'c'
  manyTill anyChar newline
  return ""

-- parses a "header" in DIMACS format: first int is number of literals
-- second int is number of clauses
headerP :: Parser (Int, Int)
headerP = do
  p <- char 'p'
  _ <- spaces
  cnf <- string "cnf"
  _  <- spaces
  numLiteral <- many1 digit
  _ <- spaces
  numClause  <- many1 digit
  return (read numLiteral, read numClause)


-- top level parser that parses an entire .cnf file
cnfFileP :: Parser (Int, Int, Formula)
cnfFileP = do
  skipMany comment
  (numLit, numClause) <- headerP
  _ <- spaces
  cnfFormula <- cnfP
  return (numLit, numClause, cnfFormula)


-- for testing with a string in terminal
parseFromString :: Parser a -> String -> Either ParseError a 
parseFromString p = runParser p () "DUMMY"




