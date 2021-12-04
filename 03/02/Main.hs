module Main where

import           Data.List          (group, maximumBy, minimumBy, sort)
import           Data.Ord           (comparing)
import           System.Environment
import           Text.Printf

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x      = (map head x) : transpose (map tail x)

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (comparing length) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = head . minimumBy (comparing length) . group . sort

rating :: (String -> Char) -> [String] -> String
rating _ [] = undefined
rating _ [x] = x
rating bitCriteria xs = cur : (rating bitCriteria $ map tail $ filter (\x -> cur == head x) xs)
    where cur = bitCriteria $ map head xs

oxygenGenRating :: [String] -> String
oxygenGenRating = rating mostCommon

co2ScrubberRating :: [String] -> String
co2ScrubberRating = rating leastCommon

binToDec :: String -> Int
binToDec = foldl (\acc x -> acc * 2 + if x == '0' then 0 else 1) 0

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath:_) -> do
      printf "Input file: %s\n" filePath
      rows <- lines <$> readFile filePath
      let oxygenGenRating = binToDec $ rating mostCommon rows
      let co2ScrubberRating = binToDec $ rating leastCommon rows
      printf "Oxygen generator rating: %d, CO2 Scrubber Rating: %d\n" oxygenGenRating co2ScrubberRating
      printf "Ans: %d\n" $ oxygenGenRating * co2ScrubberRating
    _ -> error "Expected input file"
