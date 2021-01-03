Files for SAT Solver Assignment

Copy the files in bin/ to your Linux bin/.

If you are working on platform other than Linux, you can recompile
the programs bin/walksat and bin/minisat using the source code in
src/.  You may have to edit the makefiles in order to compile
successfully.

The program bin/verify can be used to verify that your SAT solver
outputs a correct solution.  Use it as follows:

% YOUR_PROGRAM formula.cnf formula.sol

If your program determines the formula is satisfiable, then check the
solution you found:

% verify formula.cnf formula.sol

The output from verify will indicate "satisfiable" if your solution is
correct, and "unsatisfiable" if it is incorrect.

The directory Formulas/ contains the test problems.  The names of the
files indicate the size of the problem and whether it is satisfiable
or unsatisfiable.  For example,

f0020-03-u.cnf

has 20 variables, and is the 3rd unsatisfiable test problem of that
size.

Both satisfiable and unsatisfiable instances are supplied for up to
320 variables.  Only satisfiable instances are supplied for 620 and up
to 5120 variables.

