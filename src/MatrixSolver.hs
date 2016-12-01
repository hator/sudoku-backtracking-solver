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
import           "matrix" Data.Matrix hiding (trace)
import qualified "matrix" Data.Matrix as Matrix
import qualified "vector" Data.Vector as V
import qualified "vector-algorithms" Data.Vector.Algorithms.Insertion as Insertion


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
  if checkUnits cell newSudoku
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

checkUnits :: Cell -> Sudoku -> Bool
checkUnits (x,y) sudoku =
  let
    row = getRow x sudoku
    col = getCol y sudoku
    box = getMatrixAsVector $ getBox (x,y) sudoku
  in [row, col, box]
    |> map checkUnit
    |> and -- are all elements true?

getBox :: Show a => (Int,Int) -> Matrix a -> Matrix a
getBox (x,y) = submatrix x1 x2 y1 y2
  where
    x1 = (x - 1) `div` 3 * 3 + 1
    x2 = x1 + 2
    y1 = (y - 1) `div` 3 * 3 + 1
    y2 = y1 + 2

{- | Checks if given vector is a proper sudoku unit (row, col or box)
  >>> checkUnit $ V.fromList [Just 1, Just 2, Just 3, Nothing]
  True
  >>> checkUnit $ V.fromList [Just 1, Just 1, Nothing, Just 2]
  False
-}
checkUnit :: V.Vector (Maybe Value) -> Bool
checkUnit unit =
    noDuplicates && withinRange
    where
        -- values are known to contain at least 1 element (the one that we are trying to insert)
        values =
            unit
            |> V.filter isJust
            |> V.map fromJust
            |> V.modify Insertion.sort
        {-# INLINE noDuplicates #-}
        noDuplicates = isSortedBy (<) values -- strictly ascending => no duplicates
        withinRange = V.last values <= 9 && V.head values >= 1

{- | Tests if a vector is sorted by a given function.
  >>> isSortedBy (<) $ V.fromList [1,2,3,5,9]
  True
  >>> isSortedBy (<) $ V.fromList [4,3,2,1]
  False
  >>> isSortedBy (<) $ V.fromList [1,1,2,2,3]
  False
  >>> isSortedBy (<) $ V.fromList []
  True
-}
isSortedBy :: (Num a) => (a -> a -> Bool) -> V.Vector a -> Bool
isSortedBy lte = fst . V.foldl' (\(a,x) y -> (a && x `lte` y, y)) (True, 0)

