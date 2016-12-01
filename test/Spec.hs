{-# LANGUAGE PackageImports #-}

import           "hspec" Test.Hspec
import           TestData
import qualified MatrixSolver
import qualified ListSolver

import Data.Maybe

main :: IO ()
main = hspec $ do
  listSolverTests
  matrixSolverTests

listSolverTests :: Spec
listSolverTests =
  describe "ListSolver" $ do
    it "solves testSudoku1" $
      ListSolver.solve testSudoku1 `shouldBe` Just testSudoku1Answer

    it "solves testSudoku2" $
      ListSolver.solve testSudoku2 `shouldBe` Just testSudoku2Answer

    it "solves testSudoku3" $
      ListSolver.solve testSudoku3 `shouldBe` Just testSudoku3Answer

    it "solves testSudoku4" $
      ListSolver.solve testSudoku4 `shouldBe` Just testSudoku4Answer


matrixSolverTests :: Spec
matrixSolverTests =
  describe "MatrixSolver" $ do
    it "solves testSudoku1" $
      MatrixSolver.solve sudoku1 `shouldBe` Just answer1

    it "solves testSudoku2" $
      MatrixSolver.solve sudoku2 `shouldBe` Just answer2

    it "solves testSudoku3" $
      MatrixSolver.solve sudoku3 `shouldBe` Just answer3

    it "solves testSudoku4" $
      MatrixSolver.solve sudoku4 `shouldBe` Just answer4

  where
    sudoku1 = MatrixSolver.sudokuFromList testSudoku1
    sudoku2 = MatrixSolver.sudokuFromList testSudoku2
    sudoku3 = MatrixSolver.sudokuFromList testSudoku3
    sudoku4 = MatrixSolver.sudokuFromList testSudoku4
    answer1 = MatrixSolver.sudokuFromList testSudoku1Answer
    answer2 = MatrixSolver.sudokuFromList testSudoku2Answer
    answer3 = MatrixSolver.sudokuFromList testSudoku3Answer
    answer4 = MatrixSolver.sudokuFromList testSudoku4Answer
