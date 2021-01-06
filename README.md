# A SAT-Solver in Haskell
***
This repository is an implementation of a SAT solver algorithm in Haskell.

Currently the `SatSolverAssignFiles` contains some CNF formulas in the DIMACS representation. It's apparently from a CS class, whose link is [here.](https://www.cs.rochester.edu/u/kautz/Courses/444au2010/program-sat-solver.html)

Many of the test cases for this algorithm have been extracted from [here.](https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html)

### The Solvers
There are currently two modules implemented which solve SAT problems:

`BruteSolver` which is the simplest complete SAT solver you can get: a bruteforce solver. If a CNF formula contains `n` variables, then there are `2^n` possible truth assignments. The brute force solver simply enumerates each assignment, and generates the assignment, then evaluates the CNF formula. It halts if a satisfying assignment is found, otherwise it continues. If none of the `2^n` assignments work, then it returns `False`.

`RandomizedSolver` is an efficient incomplete probabilistic solver based on [the following paper.*](https://www.math.ucsd.edu/~sbuss/CourseWeb/Math268_2007WS/schoning1999.pdf) The solver uses a pure randomized Monte Carlo strategy. A parameter `maxTries` is chosen (currently fixed at 5000), and on each 'try', the algorithm uniformly at random generates truth assignment from the `2^n` that are possible. If the assignment works, the algorithm halts. Otherwise, a local search begins where a clause which is not satisfied is selected (at random) and a variable from that clause has its truth value randomly flipped, and the assignment is evaluated again. This latter loop continues for `3*n` steps. If no assignment is found during that time, we increment `maxTries` and try again with a new randomly generated truth assignment. Accoridng to the original paper, the number of `maxTries` needed is within a polynomial factor of `(2 * (1 - 1/k))^n`. I.e., `O((4/3)^n)` attempts, which is significantly faster than the bruteforce calculation of `O(2^n)` attempts.

*: Paper title: `Schöning, U. (1999), "A probabilistic algorithm for k-SAT and constraint satisfaction problems", Proceedings of 40th Annual Symposium on Foundations of Computer Science, pp. 410–414,`



`TODO: Implement a WalkSAT algorithm and/or DPLL`

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

