{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}

module Main (main) where

import           Data.List          (maximumBy)
import           Data.Ord           (comparing)
import           System.Environment
import           Text.Printf

makeStep :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
makeStep ((x, y), (dx, dy)) = ((x + dx, y + dy), (if dx == 0 then 0 else if dx > 0 then dx - 1 else dx + 1, dy - 1))

belongs :: (Int, Int, Int, Int) -> (Int, Int) -> Bool
belongs (xMin, xMax, yMin, yMax) (x, y) = x >= xMin && x <= xMax && y >= yMin && y <= yMax

landsIn :: (Int, Int, Int, Int) -> (Int, Int) -> Bool
landsIn target@(xMin, xMax, yMin, yMax) velocity@(dx, dy) = preCheck && (any (\(pos, _) -> belongs target pos) $ take 200 $ iterate makeStep ((0, 0), velocity))
  where
    preCheck = highestPosition velocity >= yMin && mostRightPosition velocity >= xMin && mostLeftPosition velocity <= xMax

highestPosition :: (Int, Int) -> Int
highestPosition (_, dy) = if dy <= 0 then 0 else (dy * (dy + 1)) `div` 2

mostRightPosition :: (Int, Int) -> Int
mostRightPosition (dx, _) = if dx <= 0 then 0 else (dx * (dx + 1)) `div` 2

mostLeftPosition :: (Int, Int) -> Int
mostLeftPosition (dx, _) = if dx >= 0 then 0 else -(dx * (dx + 1)) `div` 2

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            [xMin, xMax, yMin, yMax] <- map read . words . head . lines <$> readFile filePath :: IO [Int]
            printf "Target pos: x=%d..%d, y=%d..%d\n" xMin xMax yMin yMax
            let landingVelocities = filter (landsIn (xMin, xMax, yMin, yMax)) [(dx, dy) | dx <- [(min 0 xMin)..(max 0 xMax)], dy <- [(min 0 yMin)..500]]
            printf "Number of landing: %d\n" $ length landingVelocities
            let (bestDx, bestDy) = maximumBy (comparing snd) landingVelocities
            printf "Best velocity: (%d, %d)\n" bestDx bestDy
            printf "Ans: %d\n" $ highestPosition (bestDx, bestDy)
        _ -> error "Expected input file"
