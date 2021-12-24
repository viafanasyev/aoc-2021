{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}

module Main (main) where

import           System.Environment
import           Text.Printf

data Player = Player
    { position :: Int
    , points   :: Int
    }

data GameState = GameState
    { stepNumber :: Int
    , player1    :: Player
    , player2    :: Player
    }

spaces :: Int
spaces = 10

sides :: Int
sides = 100

pointsToWin :: Int
pointsToWin = 1000

stepNumberToSum :: Int -> Int
stepNumberToSum n = start + (start `mod` sides + 1) + ((start + 1) `mod` sides + 1)
  where
    start = (((n - 1) * 3) `mod` sides) + 1

movePlayer :: Player -> Int -> Player
movePlayer (Player pos pts) step = Player newPos (pts + newPos)
  where
    newPos = ((pos - 1) + stepNumberToSum step) `mod` spaces + 1

nextStep :: GameState -> GameState
nextStep (GameState step p1 p2) =
    if step `mod` 2 == 1
    then GameState (step + 1) (movePlayer p1 step) p2
    else GameState (step + 1) p1 (movePlayer p2 step)

play :: GameState -> GameState
play state@(GameState _ (Player _ pts1) (Player _ pts2)) =
    if pts1 >= pointsToWin || pts2 >= pointsToWin
    then state
    else play $ nextStep state

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            [pos1, pos2] <- map read . words . head . lines <$> readFile filePath :: IO [Int]
            let finalState = play $ GameState 1 (Player pos1 0) (Player pos2 0)
            let diceRolls = 3 * (stepNumber finalState - 1)
            let pts1 = points $ player1 finalState
            let pts2 = points $ player2 finalState
            printf "Dice rolls: %d, player 1 points: %d, player 2 points: %d\n" diceRolls pts1 pts2
            let loserPoints = if pts1 >= pointsToWin then pts2 else pts1
            printf "Ans: %d\n" $ loserPoints * diceRolls
        _ -> error "Expected input file"
