{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.List.Split    (splitOn)
import           System.Environment
import           Text.Printf

type Fish = Int

nextGeneration :: [Fish] -> [Fish]
nextGeneration []            = []
nextGeneration (0:fishes)    = 6 : 8 : (nextGeneration fishes)
nextGeneration (fish:fishes) = fish - 1 : (nextGeneration fishes)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            let days = 80
            fishes <- map read <$> splitOn "," <$> readFile filePath :: IO [Fish]
            let total = length $ iterate nextGeneration fishes !! days
            printf "Ans: %d\n" total
        _ -> error "Expected input file"
