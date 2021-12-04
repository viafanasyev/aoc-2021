module Main where

import           Data.List          (find)
import           Data.List.Split    (splitOn, splitWhen)
import           System.Environment
import           Text.Printf

sumOn :: (Num b) => (a -> b) -> [a] -> b
sumOn f = foldl (\acc x -> acc + f x) 0

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x      = (map head x) : transpose (map tail x)

type Board = [[(Int, Bool)]]

sumBoard :: Board -> Int
sumBoard = sum . map (sumOn fst) . map (filter (not . snd))

mark :: Int -> Board -> Board
mark _ [] = []
mark x (row:rows) = (markRow row) : mark x rows
  where
    markRow :: [(Int, Bool)] -> [(Int, Bool)]
    markRow []                   = []
    markRow ((elem, flag):elems) = (elem, flag || elem == x) : markRow elems

bingo :: Board -> Bool
bingo b = bingoRows b || bingoColumns b
  where
    bingoRows :: Board -> Bool
    bingoRows = any $ all snd
    bingoColumns :: Board -> Bool
    bingoColumns = bingoRows . transpose

lastWinning :: [Board] -> [Int] -> (Int, Board)
lastWinning _ [] = undefined
lastWinning boards (x:xs) =
  case remainingBoards of
    [] -> (x, head newBoards)
    _  -> lastWinning remainingBoards xs
  where
    newBoards = map (mark x) boards
    remainingBoards = filter (not . bingo) newBoards

readBoard :: [String] -> Board
readBoard [] = []
readBoard (row:rows) = (map (\x -> (read x, False)) $ words row) : readBoard rows

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath:_) -> do
      printf "Input file: %s\n" filePath
      rows <- lines <$> readFile filePath
      let values = map read $ splitOn "," $ head rows :: [Int]
      let boards = map readBoard $ splitWhen null $ drop 2 rows
      let (winningVal, winningBoard) = lastWinning boards values
      let winningBoardSum = sumBoard winningBoard
      printf "Winning value: %d, winning board sum: %d\n" winningVal winningBoardSum
      printf "Ans: %d\n" $ winningVal * winningBoardSum
    _ -> error "Expected input file"
