{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.Char          (digitToInt)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (fromJust, isJust)
import           System.Environment
import           Text.Printf

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate2 :: [[a]] -> [((Int, Int), a)]
enumerate2 = concat . map (\(i, row) -> map (\(j, x) -> ((i, j), x)) row) . enumerate . map enumerate

readTable :: [String] -> Map (Int, Int) Int
readTable = Map.fromList . enumerate2 . map (map digitToInt)

getNeighbours :: (Int, Int) -> Map (Int, Int) a -> [((Int, Int), a)]
getNeighbours (i, j) arr = map (\(p, x) -> (p, fromJust x)) $ filter (\(_, x) -> isJust x) $ map (\p -> (p, Map.lookup p arr)) positions
  where
    positions = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

allNeighboursHigher :: (Int, Int) -> Map (Int, Int) Int -> Bool
allNeighboursHigher pos arr = all (\(_, n) -> n > x) $ getNeighbours pos arr
  where
    x = fromJust $ Map.lookup pos arr

countRiskLevel :: Map (Int, Int) Int -> Int
countRiskLevel arr = sum $ Map.elems $ Map.mapWithKey (\pos x -> if allNeighboursHigher pos arr then x + 1 else 0) arr

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            table <- readTable . lines <$> readFile filePath
            printf "Ans: %d\n" $ countRiskLevel table
        _ -> error "Expected input file"
