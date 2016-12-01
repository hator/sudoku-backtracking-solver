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
        if checkUnits cell newSudoku
          then Just newSudoku
          else Nothing
    updateCell :: Int -> Maybe Int -> Maybe Int
    updateCell x Nothing = Just x
    updateCell _ _ = error "Should never update a already fixed cell"

checkUnits :: Cell -> Sudoku -> Bool
checkUnits cell sudoku =
  let
    row = getRow cell sudoku
    col = getCol cell sudoku
    box = getBox cell sudoku
  in [row, col, box]
    |> map checkUnit
    |> and -- check if all are True

getRow :: Cell -> Sudoku -> [Maybe Int]
getRow cell = take 9 . drop (cell `div` 9 * 9)

getCol :: Cell -> Sudoku -> [Maybe Int]
getCol cell = everynth 9 . drop (cell `mod` 9)

getBox :: Cell -> Sudoku -> [Maybe Int]
getBox cell sudoku = leftCol ++ midCol ++ rightCol
  where
    rightCol = takeCol $ drop 2 leftColSudoku
    midCol   = takeCol $ drop 1 leftColSudoku
    leftCol  = takeCol leftColSudoku
    takeCol = take 3 . everynth 9
    leftColSudoku = drop leftPad topRowSudoku
    topRowSudoku = drop (rblockNum * 3 * 9) sudoku
    leftPad = cblockNum * 3
    rblockNum = rownum `div` 3
    cblockNum = colnum `div` 3
    (rownum, colnum) = cell `divMod` 9

-- every nth element starting from first
everynth :: Int -> [a] -> [a]
everynth n [] = []
everynth n (x:xs) = x : everynth n (drop (n-1) xs)


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

