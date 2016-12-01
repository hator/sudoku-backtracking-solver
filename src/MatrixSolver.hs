{-# LANGUAGE PackageImports #-}

module MatrixSolver
  ( solve
  , sudokuFromList
  ) where

import           Prelude hiding (product)
import           Data.List hiding (group, product, transpose)
import           Data.Maybe
import           Control.Applicative (liftA2)
import           Control.Monad (join)
import           "data-ordlist" Data.List.Ordered
import           "matrix" Data.Matrix hiding (trace)
import qualified "matrix" Data.Matrix as Matrix

type Sudoku = Matrix (Maybe Int)
type Cell = (Int,Int)
type Value = Int

{-# INLINE (|>) #-}
(|>) = flip ($)

{-# INLINE for #-}
for = flip map

{-# INLINE product #-}
product = liftA2 (,)

sudokuFromList :: [a] -> Matrix a
sudokuFromList = fromList 9 9

allCells :: [Cell]
allCells = product [1..9] [1..9]

solve :: Sudoku -> Maybe (Matrix Int)
solve sudoku = join $ sequence <$> solve' sudoku allCells

solve' :: Sudoku -> [Cell] -> Maybe Sudoku
solve' sudoku [] = Just sudoku
solve' sudoku (c:cs) = solveCell sudoku cs c [1..9]

solveCell :: Sudoku -> [Cell] -> Cell -> [Value] -> Maybe Sudoku
solveCell _ _ _ [] = Nothing
solveCell sudoku cells cell (v:vs) =
    case uncurry unsafeGet cell sudoku of
      Just _ -> solve' sudoku cells
      Nothing ->
        case tryInserting sudoku cell v >>= \sud -> solve' sud cells of
          Nothing -> solveCell sudoku cells cell vs -- backtrack
          success -> success

tryInserting :: Sudoku -> Cell -> Value -> Maybe Sudoku
tryInserting sudoku cell value =
  if checkSudoku newSudoku
    then Just newSudoku
    else Nothing
  where
    newSudoku = unsafeReplaceElement (updateCell value) cell sudoku
    {-# INLINE unsafeReplaceElement #-}
    {-# INLINE updateCell #-}
    unsafeReplaceElement :: (a -> a) -> (Int, Int) -> Matrix a -> Matrix a
    unsafeReplaceElement f cell@(x,y) mat = unsafeSet (f $ unsafeGet x y mat) cell mat
    updateCell :: Int -> Maybe Int -> Maybe Int
    updateCell x Nothing = Just x
    updateCell _ j = j

checkSudoku :: Sudoku -> Bool
checkSudoku sudoku =
  let
    rows = toLists sudoku
    cols = toLists $ transpose sudoku
    boxes = map toList $ toBoxes sudoku
  in [rows, cols, boxes]
    |> map (map checkUnit)
    |> concat
    |> all (True ==)

allBoxesCoords :: [(Cell, Cell)]
allBoxesCoords = product coords coords
  where coords = [(1,3), (4,6), (7,9)]

toBoxes :: Matrix a -> [Matrix a]
toBoxes sudoku =
  for allBoxesCoords $
    uncurry $ uncurry.uncurry submatrixSudoku
  where submatrixSudoku a b c d = submatrix a b c d sudoku

checkUnit :: [Maybe Value] -> Bool
checkUnit unit =
    noDuplicates && withinRange
    where
        -- values are known to contain at least 1 element (the one that we are trying to insert)
        values =
            unit
            |> filter isJust
            |> map fromJust
            |> sort
        {-# INLINE noDuplicates #-}
        noDuplicates = isSortedBy (<) values -- strictly ascending => no duplicates
        withinRange = last values <= 9 && head values >= 1

