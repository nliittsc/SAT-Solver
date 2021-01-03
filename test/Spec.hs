import SAT

main :: IO ()
main = do
  contents <- readFile "test\\test-formulas\\simple.cnf"
  let input = map words $ lines contents
  print input

