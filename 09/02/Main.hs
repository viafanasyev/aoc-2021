{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.Char          (digitToInt)
import           Data.List          (sort)
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

basin :: (Int, Int) -> Map (Int, Int) Int -> [(Int, Int)]
basin pos arr = case Map.lookup pos arr of
    Nothing -> []
    Just 9  -> []
    Just _  -> if allNeighboursHigher pos arr then basinHelper pos arr [] else []
  where
    basinHelper :: (Int, Int) -> Map (Int, Int) Int -> [(Int, Int)] -> [(Int, Int)]
    basinHelper pos arr visited = res
      where
        x = fromJust $ Map.lookup pos arr
        res = foldl (\acc n -> tryVisit n acc) (pos : visited) $ getNeighbours pos arr
          where
            tryVisit :: ((Int, Int), Int) -> [(Int, Int)] -> [(Int, Int)]
            tryVisit (p, v) visited = if not (p `elem` visited) && x < v && v /= 9 then basinHelper p arr visited else visited

basinSize :: (Int, Int) -> Map (Int, Int) Int -> Int
basinSize pos arr = length $ basin pos arr

basinsSizes :: Map (Int, Int) Int -> [Int]
basinsSizes arr = Map.elems $ Map.mapWithKey (\pos _ -> basinSize pos arr) arr

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            table <- readTable . lines <$> readFile filePath
            printf "Ans: %d\n" $ foldl (*) 1 $ take 3 $ reverse $ sort $ basinsSizes table
        _ -> error "Expected input file"
