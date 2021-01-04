# A SAT-Solver in Haskell
***
This repository is an implementation of a SAT solver algorithm in Haskell.

Currently the `SatSolverAssignFiles` contains some CNF formulas in the DIMACS representation. It's apparently from a CS class, whose link is [here.](https://www.cs.rochester.edu/u/kautz/Courses/444au2010/program-sat-solver.html)

Many of the test cases for this algorithm have been extracted from [here.](https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html)

### The Solver
`TODO`: Implement bruteforce solver for small formulas

### Types
Currently the solver uses a recursive data type
`TODO`: Put examples

### Test Generation
`TODO:` Write a generator for *Uniform Random 3-SAT* tests based on the following description:

`Uniform Random-3-SAT is a family of SAT problems distributions obtained by randomly generating 3-CNF formulae in the following way: For an instance with n variables and k clauses, each of the k clauses is constructed from 3 literals which are randomly drawn from the 2n possible literals (the n variables and their negations) such that each possible literal is selected with the same probability of 1/2n. Clauses are not accepted for the construction of the problem instance if they contain multiple copies of the same literal or if they are tautological (i.e., they contain a variable and its negation as a literal). Each choice of n and k thus induces a distribution of Random-3-SAT instances. Uniform Random-3-SAT is the union of these distributions over all n and k.`

More can be found [here.](https://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/RND3SAT/descr.html)


### Parsing
The solver currently expects a file to be in standard DIMACs format, as described [in this link.](https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html)

In order to parse the file into a data structure that can be used by the solver, the [parsec library](https://github.com/haskell/parsec) is used. The source code for the parsing operations can be found in `src/ParseCNF.hs`. 

