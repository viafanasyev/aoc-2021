module Main where

import Data.List (tails)
import System.Environment
import Text.Printf

zipWithNext :: [a] -> [(a, a)]
zipWithNext xs = zip xs $ tail xs

count :: (a -> Bool) -> [a] -> Int
count p = foldl (\acc x -> if p x then acc + 1 else acc) 0

countIncreases :: [Int] -> Int
countIncreases = count (\(cur, next) -> cur < next) . zipWithNext

windows :: Int -> [Int] -> [[Int]]
windows k = foldr (zipWith (:)) (repeat []) . take k . tails

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath:_) -> do
      printf "Input file: %s\n" filePath
      depths <- map (read :: String -> Int) . lines <$> readFile filePath
      printf "The number of times the sum of measurements in sliding window increases: %d\n" $ countIncreases . map sum . windows 3 $ depths
    _ -> error "Expected input file"
