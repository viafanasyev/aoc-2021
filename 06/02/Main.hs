{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.List.Split    (splitOn)
import           System.Environment
import           Text.Printf

type Fish = Int

-- F(X) - number of fishes after X days if only one fish with timer=0 exists
-- F(x) = F(x - 7) + F(x - 9)
memoizedFishes :: [Int]
memoizedFishes = 1 : 1 : 2 : 2 : 2 : 2 : 2 : 2 : 2 : zipWith (+) memoizedFishes (drop 2 memoizedFishes)

memoizedFishesForDays :: Int -> [Int]
memoizedFishesForDays days = reverse $ drop (days - 6) $ take (days + 2) memoizedFishes

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            let days = 256
            let memo = memoizedFishesForDays days
            fishes <- map read <$> splitOn "," <$> readFile filePath :: IO [Fish]
            let total = sum $ map (memo !!) fishes
            printf "Ans: %d\n" total
        _ -> error "Expected input file"
