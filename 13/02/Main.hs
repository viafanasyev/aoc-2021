{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.List          (nub)
import           Data.List.Split    (splitOn, splitWhen)
import           System.Environment
import           Text.Printf

type Point = (Int, Int)

data Fold = FoldX Int | FoldY Int

readPoint :: String -> Point
readPoint s = (x, y)
  where
    [x, y] = map read $ splitOn "," s

readFold :: String -> Fold
readFold s = if axis == 'x' then FoldX pos else FoldY pos
  where
    [rawAxis, rawPos] = splitOn "=" s
    axis = last rawAxis
    pos = read rawPos

makeFold :: Fold -> [Point] -> [Point]
makeFold (FoldX foldPos) = nub . map (\(x, y) -> if x < foldPos then (x, y) else (2 * foldPos - x, y))
makeFold (FoldY foldPos) = nub . map (\(x, y) -> if y < foldPos then (x, y) else (x, 2 * foldPos - y))

displayPoints :: [Point] -> IO ()
displayPoints points = do
    let (maxX, maxY) = foldl (\(accX, accY) (curX, curY) -> (max accX curX, max accY curY)) (0, 0) points
    mapM_ putChar [if x > maxX then '\n' else if (x, y) `elem` points then '#' else '.' | y <- [0..maxY], x <- [0..maxX + 1]]

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            rows <- lines <$> readFile filePath
            let [rawPoints, rawFolds] = splitWhen null rows
            let points = map readPoint rawPoints
            let folds = map readFold rawFolds
            let res = foldl (flip makeFold) points folds
            displayPoints res
        _ -> error "Expected input file"
