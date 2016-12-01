{-# LANGUAGE PackageImports #-}

module ListSolver
  ( solve
  ) where

import           Data.List hiding (group)
import           "data-ordlist" Data.List.Ordered
import           Control.Monad (join)
import           Data.Maybe
import           "lens" Control.Lens.At
import           "lens" Control.Lens.Operators ((&), (^?!))
import           "lens" Control.Lens.Getter
import           "lens" Control.Lens.Setter
import           "split" Data.List.Split

type List a = [a]
type Sudoku = List (Maybe Int)

{-# INLINE (|>) #-}
(|>) = flip ($)

type Cell = Int
type Value = Int

solve :: Sudoku -> Maybe [Int]
solve sudoku = join $ sequence <$> solve' sudoku [0..80]

solve' :: Sudoku -> List Cell -> Maybe Sudoku
solve' sudoku [] = Just sudoku
solve' sudoku (c:cs) = solveCell sudoku cs c [1..9]

solveCell :: Sudoku -> List Cell -> Cell -> List Value -> Maybe Sudoku
solveCell _ _ _ [] = Nothing
solveCell sudoku cells cell (v:vs) =
    case currentCellValue of
      Just _ -> solve' sudoku cells
      Nothing ->
        case tryInserting sudoku cell v >>= \sud -> solve' sud cells of
          Nothing -> solveCell sudoku cells cell vs -- backtrack
          success -> success
  where
    currentCellValue = sudoku ^?! ix cell
    tryInserting :: Sudoku -> Cell -> Value -> Maybe Sudoku
    tryInserting sudoku cell value =
      let newSudoku = sudoku & ix cell %~ updateCell value
      in
        if checkSudoku newSudoku
          then Just newSudoku
          else Nothing
    updateCell :: Int -> Maybe Int -> Maybe Int
    updateCell x Nothing = Just x
    updateCell _ _ = error "Should never update a already fixed cell"

checkSudoku :: Sudoku -> Bool
checkSudoku sudoku =
  let
    rows = chunksOf 9 sudoku
    cols = transpose rows
    boxes = toBoxes rows
  in [rows, cols, boxes]
    |> map (map checkUnit)
    |> concat
    |> all (True ==)

toBoxes :: List (List a) -> List (List a)
toBoxes sudoku =
  sudoku
  |> map (chunksOf 3)
  |> concat
  |> group
  |> map concat

group :: List a -> List (List a)
group list = group_ list [] [] [] []
group_ qs xs ys zs result =
  case qs of
    [] -> xs:ys:zs:result
    q:qs ->
      if length xs == 3
        then group_ qs zs [q] ys (xs:result)
        else group_ qs zs (q:xs) ys result

checkUnit :: List (Maybe Value) -> Bool
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

