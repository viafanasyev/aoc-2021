{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}

module Main (main) where

import           Data.Map           (Map)
import qualified Data.Map           as Map
import           System.Environment
import           Text.Printf

data Player = Player
    { position :: Int
    , points   :: Int
    } deriving stock (Eq, Ord, Show)

spaces :: Int
spaces = 10

pointsToWin :: Int
pointsToWin = 21

transitions :: [(Int, Int)]
transitions = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

movePlayer :: Player -> Int -> Player
movePlayer (Player pos pts) delta = Player newPos (pts + newPos)
  where
    newPos = ((pos - 1) + delta) `mod` spaces + 1

winsNumber :: Player -> Player -> Map (Player, Player) (Int, Int) -> (Map (Player, Player) (Int, Int), Int, Int)
winsNumber p1 p2 cache =
    if points p2 >= pointsToWin
    then (cache, 0, 1)
    else
        if points p1 == pointsToWin - 1
        then (cache, 27, 0)
        else case Map.lookup (p1, p2) cache of
            Just (res1, res2) -> (cache, res1, res2)
            Nothing -> let (newCache, newRes1, newRes2) = foldl (makeTransition p1 p2) (cache, 0, 0) transitions in (Map.insert (p1, p2) (newRes1, newRes2) newCache, newRes1, newRes2)

makeTransition :: Player -> Player -> (Map (Player, Player) (Int, Int), Int, Int) -> (Int, Int) -> (Map (Player, Player) (Int, Int), Int, Int)
makeTransition p1 p2 (oldCache, oldRes1, oldRes2) (inc, times) = (Map.union oldCache newCache, oldRes1 + newRes1 * times, oldRes2 + newRes2 * times)
  where
    (newCache, newRes2, newRes1) = winsNumber p2 (movePlayer p1 inc) oldCache

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            [pos1, pos2] <- map read . words . head . lines <$> readFile filePath :: IO [Int]
            let (_, pts1, pts2) = winsNumber (Player pos1 0) (Player pos2 0) Map.empty
            printf "Player 1 wins: %d, player 2 wins: %d\n" pts1 pts2
            printf "Ans: %d\n" $ if pts1 > pts2 then pts1 else pts2
        _ -> error "Expected input file"
