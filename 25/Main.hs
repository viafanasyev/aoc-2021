{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}

module Main (main) where

import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (fromJust)
import           System.Environment
import           Text.Printf

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate2 :: [[a]] -> [((Int, Int), a)]
enumerate2 = concat . map (\(i, row) -> map (\(j, x) -> ((i, j), x)) row) . enumerate . map enumerate

readTable :: [String] -> Map (Int, Int) Char
readTable = Map.fromList . enumerate2

makeStep :: Map (Int, Int) Char -> Map (Int, Int) Char
makeStep = makeSouthStep . makeEastStep
  where
    makeEastStep :: Map (Int, Int) Char -> Map (Int, Int) Char
    makeEastStep oldRegion = foldl (\curRegion pos -> if (fromJust $ Map.lookup pos oldRegion) == '>' && rightNeighbour pos == '.' then moveRight curRegion pos else curRegion) oldRegion positions
      where
        positions = Map.keys oldRegion
        rightNeighbour :: (Int, Int) -> Char
        rightNeighbour (y, x) = case Map.lookup (y, x + 1) oldRegion of
            Just c  -> c
            Nothing -> fromJust $ Map.lookup (y, 0) oldRegion
        moveRight :: Map (Int, Int) Char -> (Int, Int) -> Map (Int, Int) Char
        moveRight region pos@(y, x) = Map.insert pos '.' $ case Map.member (y, x + 1) region of
            True  -> Map.insert (y, x + 1) '>' region
            False -> Map.insert (y, 0) '>' region
    makeSouthStep :: Map (Int, Int) Char -> Map (Int, Int) Char
    makeSouthStep oldRegion = foldl (\curRegion pos -> if (fromJust $ Map.lookup pos oldRegion) == 'v' && bottomNeighbour pos == '.' then moveDown curRegion pos else curRegion) oldRegion positions
      where
        positions = Map.keys oldRegion
        bottomNeighbour :: (Int, Int) -> Char
        bottomNeighbour (y, x) = case Map.lookup (y + 1, x) oldRegion of
            Just c  -> c
            Nothing -> fromJust $ Map.lookup (0, x) oldRegion
        moveDown :: Map (Int, Int) Char -> (Int, Int) -> Map (Int, Int) Char
        moveDown region pos@(y, x) = Map.insert pos '.' $ case Map.member (y + 1, x) region of
            True  -> Map.insert (y + 1, x) 'v' region
            False -> Map.insert (0, x) 'v' region

lastStep :: Map (Int, Int) Char -> (Int, Map (Int, Int) Char)
lastStep region = if region == newRegion then (1, region) else let (lastStepNum, lastStepRegion) = lastStep newRegion in (lastStepNum + 1, lastStepRegion)
  where
    newRegion = makeStep region

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            table <- readTable . lines <$> readFile filePath
            printf "Ans: %d\n" $ fst $ lastStep table
        _ -> error "Expected input file"
