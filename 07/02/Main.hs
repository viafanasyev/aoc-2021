{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.List.Split    (splitOn)
import           System.Environment
import           Text.Printf

type Crab = Int

costOfAlignTo :: Int -> [Crab] -> Int
costOfAlignTo x = foldl (\acc cur -> acc + (costOfMoveBy $ abs $ x - cur)) 0
  where
    costOfMoveBy :: Int -> Int
    costOfMoveBy k = ((k + 1) * k) `div` 2

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            crabs <- map read <$> splitOn "," <$> readFile filePath :: IO [Crab]
            let ans = minimum $ map (\x -> costOfAlignTo x crabs) [minimum crabs .. maximum crabs]
            printf "Ans: %d\n" ans
        _ -> error "Expected input file"
