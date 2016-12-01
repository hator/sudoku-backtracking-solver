Sudoku backtracking solver
==========================

Simple implementation of a backtracking solver.

Includes 2 versions: based on lists and based on matrices from matrix package.

Installation
------------

    $ git clone https://github.com/hator/sudoku-backtracking-solver.git
    $ cd sudoku-backtracking-solver
    $ stack install

Tests:

    $ stack test

Benchmark:
    $ stack bench

Usage
-----
```haskell
import Sudoku.ListSolver (solve)

sudoku :: [Maybe Int]
sudoku = -- your sudoku

solved :: Maybe [Int]
solved = solve sudoku
```

```haskell
import Sudoku.MatrixSolver (solve, sudokuFromList)

sudoku :: [Maybe Int]
sudoku = -- your sudoku

solved :: Maybe (Matrix Int)
solved = solve $ sudokuFromList sudoku
```

See `test/Spec.hs` for examples.
