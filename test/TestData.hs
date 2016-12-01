module TestData
    ( testSudoku1
    , testSudoku1Answer
    , testSudoku2
    , testSudoku2Answer
    , testSudoku3
    , testSudoku3Answer
    , testSudoku4
    , testSudoku4Answer
    ) where

zeroToNothing :: (Num a, Eq a) => a -> Maybe a
zeroToNothing 0 = Nothing
zeroToNothing x = Just x


-- 79-clue sudoku
testSudoku1 :: (Num n, Eq n) => [Maybe n]
testSudoku1 = map zeroToNothing
  [ 7,2,6, 4,9,3, 8,1,5
  , 3,1,5, 7,2,8, 9,4,6
  , 4,8,9, 6,5,1, 2,3,7

  , 8,5,2, 1,4,7, 6,9,3
  , 6,7,3, 9,8,5, 1,2,4
  , 9,4,1, 3,6,2, 7,5,8

  , 1,9,4, 8,3,6, 5,7,2
  , 5,6,7, 2,1,4, 3,8,9
  , 2,3,8, 5,7,9, 4,0,0
  ]

testSudoku1Answer :: (Num n, Eq n) => [Maybe n]
testSudoku1Answer = map return
  [ 7,2,6, 4,9,3, 8,1,5
  , 3,1,5, 7,2,8, 9,4,6
  , 4,8,9, 6,5,1, 2,3,7

  , 8,5,2, 1,4,7, 6,9,3
  , 6,7,3, 9,8,5, 1,2,4
  , 9,4,1, 3,6,2, 7,5,8

  , 1,9,4, 8,3,6, 5,7,2
  , 5,6,7, 2,1,4, 3,8,9
  , 2,3,8, 5,7,9, 4,6,1
  ]


-- 26-clue sudoku
testSudoku2 :: (Num n, Eq n) => [Maybe n]
testSudoku2 = map zeroToNothing
  [ 5,0,0, 0,8,0, 0,4,9
  , 0,0,0, 5,0,0, 0,3,0
  , 0,6,7, 3,0,0, 0,0,1

  , 1,5,0, 0,0,0, 0,0,0
  , 0,0,0, 2,0,8, 0,0,0
  , 0,0,0, 0,0,0, 0,1,8

  , 7,0,0, 0,0,4, 1,5,0
  , 0,3,0, 0,0,2, 0,0,0
  , 4,9,0, 0,5,0, 0,0,3
  ]

testSudoku2Answer :: (Num n, Eq n) => [Maybe n]
testSudoku2Answer = map return
  [ 5,1,3, 6,8,7, 2,4,9
  , 8,4,9, 5,2,1, 6,3,7
  , 2,6,7, 3,4,9, 5,8,1

  , 1,5,8 ,4,6,3, 9,7,2
  , 9,7,4, 2,1,8, 3,6,5
  , 3,2,6, 7,9,5, 4,1,8

  , 7,8,2, 9,3,4, 1,5,6
  , 6,3,5, 1,7,2, 8,9,4
  , 4,9,1, 8,5,6, 7,2,3
  ]


-- 24-clue sudoku
testSudoku3 :: (Num n, Eq n) => [Maybe n]
testSudoku3 = map zeroToNothing
  [ 0,0,6, 0,0,8, 5,0,0
  , 0,0,0, 0,7,0, 6,1,3
  , 0,0,0, 0,0,0, 0,0,9

  , 0,0,0, 0,9,0, 0,0,1
  , 0,0,1, 0,0,0, 8,0,0
  , 4,0,0, 5,3,0, 0,0,0

  , 1,0,7, 0,5,3, 0,0,0
  , 0,5,0, 0,6,4, 0,0,0
  , 3,0,0, 1,0,0, 0,6,0
  ]

testSudoku3Answer :: (Num n, Eq n) => [Maybe n]
testSudoku3Answer = map return
  [ 2,9,6, 3,1,8, 5,7,4
  , 5,8,4, 9,7,2, 6,1,3
  , 7,1,3, 6,4,5, 2,8,9

  , 6,2,5, 8,9,7, 3,4,1
  , 9,3,1, 4,2,6, 8,5,7
  , 4,7,8, 5,3,1, 9,2,6

  , 1,6,7, 2,5,3, 4,9,8
  , 8,5,9, 7,6,4, 1,3,2
  , 3,4,2, 1,8,9, 7,6,5
  ]


-- 17-clue sudoku
testSudoku4 :: (Num n, Eq n) => [Maybe n]
testSudoku4 = map zeroToNothing
  [ 0,0,0, 7,0,0, 0,0,0
  , 1,0,0, 0,0,0, 0,0,0
  , 0,0,0, 4,3,0, 2,0,0

  , 0,0,0, 0,0,0, 0,0,6
  , 0,0,0, 5,0,9, 0,0,0
  , 0,0,0, 0,0,0, 4,1,8

  , 0,0,0, 0,8,1, 0,0,0
  , 0,0,2, 0,0,0, 0,5,0
  , 0,4,0, 0,0,0, 3,0,0
  ]

testSudoku4Answer :: (Num n, Eq n) => [Maybe n]
testSudoku4Answer = map return
  [ 2,6,4, 7,1,5, 8,3,9
  , 1,3,7, 8,9,2, 6,4,5
  , 5,9,8, 4,3,6, 2,7,1

  , 4,2,3, 1,7,8, 5,9,6
  , 8,1,6, 5,4,9, 7,2,3
  , 7,5,9, 6,2,3, 4,1,8

  , 3,7,5, 2,8,1, 9,6,4
  , 9,8,2, 3,6,4, 1,5,7
  , 6,4,1, 9,5,7, 3,8,2
  ]