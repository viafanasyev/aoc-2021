{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}

module Main (main) where

import           Data.Char          (digitToInt)
import           Data.List          (minimumBy)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (fromJust, isJust)
import           Data.Ord           (comparing)
import           System.Environment
import           Text.Printf

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate2 :: [[a]] -> [((Int, Int), a)]
enumerate2 = concat . map (\(i, row) -> map (\(j, x) -> ((i, j), x)) row) . enumerate . map enumerate

readTable :: [String] -> Map (Int, Int) Int
readTable = Map.fromList . enumerate2 . map (map digitToInt)

expandTable :: Map (Int, Int) Int -> Map (Int, Int) Int
expandTable table = foldl (\newMap (pos, v) -> expandCell pos v newMap) Map.empty $ Map.toList table
  where
    (lastRow, lastColumn) = fst $ Map.findMax table
    (rowsNumber, columnsNumber) = (lastRow + 1, lastColumn + 1)
    expandCell :: (Int, Int) -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int
    expandCell (y, x) v resMap = foldl (\res (pos, value) -> Map.insert pos value res) resMap [((y + rowsNumber * dy, x + columnsNumber * dx), (v + dy + dx - 1) `mod` 9 + 1) | dy <- [0..4], dx <- [0..4]]

class WeightedGraph g where
    neighbours :: (Ord a) => g a -> Vertex a -> [(Vertex a, Int)]

newtype MapWeightedGraph a = AdjacencyMap (Map (Vertex a) [(Vertex a, Int)])

instance WeightedGraph MapWeightedGraph where
    neighbours (AdjacencyMap adjacent) v = Map.findWithDefault [] v adjacent

newtype Vertex a = Named a
    deriving stock (Ord, Eq, Show)

tableToGraph :: Map (Int, Int) Int -> MapWeightedGraph (Int, Int)
tableToGraph table = AdjacencyMap $ Map.mapKeys Named $ Map.mapWithKey (\cell _ -> neighboursOf cell) table
  where
    neighboursOf :: (Int, Int) -> [(Vertex (Int, Int), Int)]
    neighboursOf (i, j) = map fromJust $ filter isJust [top, bottom, left, right]
      where
        top    = case (Map.lookup (i - 1, j) table) of Nothing -> Nothing; Just d -> Just (Named (i - 1, j), d)
        bottom = case (Map.lookup (i + 1, j) table) of Nothing -> Nothing; Just d -> Just (Named (i + 1, j), d)
        left   = case (Map.lookup (i, j - 1) table) of Nothing -> Nothing; Just d -> Just (Named (i, j - 1), d)
        right  = case (Map.lookup (i, j + 1) table) of Nothing -> Nothing; Just d -> Just (Named (i, j + 1), d)

dijkstra :: (WeightedGraph g, Ord a) => g a -> Vertex a -> Map (Vertex a) Int
dijkstra g start = helper g (Map.singleton start 0) (Map.singleton start 0)
  where
    helper :: (WeightedGraph g, Ord a) => g a -> Map (Vertex a) Int -> Map (Vertex a) Int -> Map (Vertex a) Int
    helper graph queue distances = case (if null queue then Nothing else Just $ findMinValueBy (comparing snd) queue) of
        Nothing -> distances
        Just (curV, curD) -> uncurry (helper graph) $ foldl updateQueue'n'Dists ((Map.delete curV queue), distances) (neighbours graph curV)
          where
            updateQueue'n'Dists :: (Ord a) => (Map (Vertex a) Int, Map (Vertex a) Int) -> (Vertex a, Int) -> (Map (Vertex a) Int, Map (Vertex a) Int)
            updateQueue'n'Dists (q, d) (nbV, nbD) = case Map.lookup nbV d of
                Nothing -> (Map.insert nbV (curD + nbD) q, Map.insert nbV (curD + nbD) d)
                Just prevD -> if curD + nbD < prevD then (Map.insert nbV (curD + nbD) q, Map.insert nbV (curD + nbD) d) else (q, d)

findMinValueBy :: ((k, a) -> (k, a) -> Ordering) -> Map k a -> (k, a)
findMinValueBy comp = minimumBy comp . Map.toList

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            rows <- lines <$> readFile filePath
            let rowsNumber = 5 * (length rows)
            let columnsNumber = 5 * (length (rows !! 0))
            let graph = tableToGraph $ expandTable $ readTable rows
            printf "Ans: %d\n" $ dijkstra graph (Named (0, 0)) Map.! Named (rowsNumber - 1, columnsNumber - 1)
        _ -> error "Expected input file"
