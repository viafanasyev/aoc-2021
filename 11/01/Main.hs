{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.Char          (digitToInt)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           System.Environment
import           Text.Printf

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate2 :: [[a]] -> [((Int, Int), a)]
enumerate2 = concat . map (\(i, row) -> map (\(j, x) -> ((i, j), x)) row) . enumerate . map enumerate

readTable :: [String] -> Map (Int, Int) Int
readTable = Map.fromList . enumerate2 . map (map digitToInt)

neighboursOf :: (Int, Int) -> [(Int, Int)]
neighboursOf (i, j) =
    [ (i - 1, j - 1)
    , (i - 1, j    )
    , (i - 1, j + 1)
    , (i    , j - 1)
    , (i    , j    )
    , (i    , j + 1)
    , (i + 1, j - 1)
    , (i + 1, j    )
    , (i + 1, j + 1)
    ]

filterFlashing :: Map (Int, Int) Int -> [(Int, Int)]
filterFlashing = map fst . filter ((>9) . snd) . Map.toList

flashOne :: Map (Int, Int) (Bool, Int) -> (Int, Int) -> Map (Int, Int) (Bool, Int)
flashOne table pos = case Map.lookup pos table of
    Nothing -> table
    Just (flag, value) -> if flag || value <= 9 then table else flashNeighbours $ updateNeighbours $ Map.update (\_ -> Just (True, value)) pos table
      where
        neighbours = neighboursOf pos
        updateNeighbours :: Map (Int, Int) (Bool, Int) -> Map (Int, Int) (Bool, Int)
        updateNeighbours = flip (foldl (flip $ Map.update (\(fl, val) -> if fl then Just (fl, val) else Just (fl, val + 1)))) neighbours
        flashNeighbours :: Map (Int, Int) (Bool, Int) -> Map (Int, Int) (Bool, Int)
        flashNeighbours = flip (foldl flashOne) neighbours

flashAll :: Map (Int, Int) Int -> Map (Int, Int) (Bool, Int)
flashAll table = foldl flashOne (Map.map ((,) False) table) (filterFlashing table)

step :: Map (Int, Int) Int -> (Int, Map (Int, Int) Int)
step = Map.mapAccum (\acc (fl, v) -> if fl then (acc + 1, 0) else (acc, v)) 0 . flashAll . Map.map (+1)

simulate :: Int -> Map (Int, Int) Int -> Int
simulate 0 _ = 0
simulate x table = (numOfFlashes) + (simulate (x - 1) newTable)
  where
    (numOfFlashes, newTable) = step table

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            table <- readTable . lines <$> readFile filePath
            printf "Ans: %d\n" $ simulate 100 table
        _ -> error "Expected input file"
