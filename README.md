# sudoku-solver
Two Sudoku solvers written in Scala:
one uses simple backtracking to find a solution;
the other parallelises the search using the AKKA framework.

Usage:
* `scala Solver [filename]` (if no filename is given, the puzzle is read from stdin), or
* `scala PSolver [filename]` (for the parallelised version)
