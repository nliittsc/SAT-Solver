# A SAT-Solver in Haskell
***
This repository is an implementation of a SAT solver algorithm in Haskell.


Many of the test cases for this algorithm have been extracted from [here.](https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html)

### The Solvers
There are currently two modules implemented which solve SAT problems:

`BruteSolver` which is the simplest complete SAT solver you can get: a bruteforce solver. If a CNF formula contains `n` variables, then there are `2^n` possible truth assignments. The brute force solver simply enumerates each assignment, and generates the assignment, then evaluates the CNF formula. It halts if a satisfying assignment is found, otherwise it continues. If none of the `2^n` assignments work, then it returns `False`.

`RandomizedSolver` is an efficient incomplete probabilistic solver based on [the following paper.*](https://www.math.ucsd.edu/~sbuss/CourseWeb/Math268_2007WS/schoning1999.pdf) The solver uses a pure randomized Monte Carlo strategy. A parameter `maxTries` is chosen (currently fixed at 5000), and on each 'try', the algorithm uniformly at random generates truth assignment from the `2^n` that are possible. If the assignment works, the algorithm halts. Otherwise, a local search begins where a clause which is not satisfied is selected (at random) and a variable from that clause has its truth value randomly flipped, and the assignment is evaluated again. This latter loop continues for `3*n` steps. If no assignment is found during that time, we increment `maxTries` and try again with a new randomly generated truth assignment. Accoridng to the original paper, the number of `maxTries` needed is within a polynomial factor of `(2 * (1 - 1/k))^n`. I.e., `O((4/3)^n)` attempts, which is significantly faster than the bruteforce calculation of `O(2^n)` attempts.

*: Paper title: `Schöning, U. (1999), "A probabilistic algorithm for k-SAT and constraint satisfaction problems", Proceedings of 40th Annual Symposium on Foundations of Computer Science, pp. 410–414,`



`TODO: Implement a WalkSAT algorithm and/or DPLL`

### Types and Data Structures
The Types here are extremely simple. The idea here is that we want to parse a `.cnf` file (DIMACS format) as literally as possible. So, formulas are read from the file and manipulated by the solver in a literal way. E.g., a `Literal` is really just an `Int`. So if `x :: Int` and `x > 0 == True`, then `x` is a positive literal, and if `x < 0 == True` then `x` is a negative literal (negating whatever its truth assignment is.) Clauses then are just `[Literal]` and a `Formula` is really just `[Clause]`.

Truth assignments are stored in an `Data.IntMap`, which allows pseudo-constant time look ups and modification. Combined with Haskell's efficient `map`s over lists, a formula in Conjunctive Normal Form that has `N` clauses should evaluate in `O(N)` time (one the number of variables is large enough).

`TODO:` Adjust data structures and parsing, so that variables in the `.cnf` file are not required to be contiguous. E.g., one could pass `1 5 3 0` as a clause instead of `1 2 3 0`. Strategy: get a list of variables directly from the parsing process, instead of just the number of variables

### Test Generation
`TODO:` Write a generator for *Uniform Random 3-SAT* tests based on the following description:

`Uniform Random-3-SAT is a family of SAT problems distributions obtained by randomly generating 3-CNF formulae in the following way: For an instance with n variables and k clauses, each of the k clauses is constructed from 3 literals which are randomly drawn from the 2n possible literals (the n variables and their negations) such that each possible literal is selected with the same probability of 1/2n. Clauses are not accepted for the construction of the problem instance if they contain multiple copies of the same literal or if they are tautological (i.e., they contain a variable and its negation as a literal). Each choice of n and k thus induces a distribution of Random-3-SAT instances. Uniform Random-3-SAT is the union of these distributions over all n and k.`

More can be found [here.](https://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/RND3SAT/descr.html)

I think `QuickCheck` can be used for this?


### Parsing
The solver currently expects a file to be in standard DIMACs format, as described [in this link.](https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html)

In order to parse the file into a data structure that can be used by the solver, the [parsec library](https://github.com/haskell/parsec) is used. The source code for the parsing operations can be found in `src/ParseCNF.hs`. 

The parsing is basic and done very 'literally' in that the `.cnf` file essentially just encodes the a list of lists of `Literal`s (which are just `Int`s.)