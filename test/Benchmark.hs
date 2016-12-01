{-# LANGUAGE PackageImports #-}

import           "criterion" Criterion (Benchmark, bench, whnf)
import           "criterion" Criterion.Main (bgroup, defaultMain)
import           TestData
import qualified ListSolver
import qualified MatrixSolver

main :: IO ()
main = defaultMain
  [ bgroup "ListSolver" listSolverBench
  , bgroup "MatrixSolver" matrixSolverBench
  ]

listSolverBench :: [Benchmark]
listSolverBench =
  [ bench "sudoku1" $ whnf ListSolver.solve testSudoku1
  , bench "sudoku2" $ whnf ListSolver.solve testSudoku2
  , bench "sudoku3" $ whnf ListSolver.solve testSudoku3
  ]

matrixSolverBench :: [Benchmark]
matrixSolverBench =
  [ bench "sudoku1" $ whnf MatrixSolver.solve (MatrixSolver.sudokuFromList testSudoku1)
  , bench "sudoku2" $ whnf MatrixSolver.solve (MatrixSolver.sudokuFromList testSudoku2)
  , bench "sudoku3" $ whnf MatrixSolver.solve (MatrixSolver.sudokuFromList testSudoku3)
  ]
