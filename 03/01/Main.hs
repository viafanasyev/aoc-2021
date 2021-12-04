module Main where

import Data.List (group, maximumBy, sort)
import Data.Ord (comparing)
import System.Environment
import Text.Printf

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (comparing length) . group . sort

invertBinary :: String -> String
invertBinary [] = []
invertBinary ('0':xs) = '1' : invertBinary xs
invertBinary ('1':xs) = '0' : invertBinary xs

binToDec :: String -> Int
binToDec = foldl (\acc x -> acc * 2 + if x == '0' then 0 else 1) 0

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath:_) -> do
      printf "Input file: %s\n" filePath
      gammaBin <- map mostCommon . transpose . lines <$> readFile filePath
      let epsilonBin = invertBinary gammaBin
      let gamma = binToDec gammaBin
      let epsilon = binToDec epsilonBin
      printf "Gamma: %d, Eps: %d\n" gamma epsilon
      printf "Ans: %d\n" $ gamma * epsilon
    _ -> error "Expected input file"
