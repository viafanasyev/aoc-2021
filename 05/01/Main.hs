{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.List          (intersect, nub, tails)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (maybeToList)
import           System.Environment
import           Text.Printf

type Point2D = (Int, Int)
type Segment2D = (Point2D, Point2D)

type Point1D = Int
type Segment1D = (Point1D, Point1D)

horizontal :: Segment2D -> Bool
horizontal ((_, yStart), (_, yEnd)) = yStart == yEnd

vertical :: Segment2D -> Bool
vertical ((xStart, _), (xEnd, _)) = xStart == xEnd

validSegment2D :: Segment2D -> Bool
validSegment2D s = horizontal s || vertical s

intersect1D :: Segment1D -> Segment1D -> [Point1D]
intersect1D (start1, end1) (start2, end2) = intersect s1 s2
  where
    s1 = if start1 < end1 then [start1..end1] else [end1..start1]
    s2 = if start2 < end2 then [start2..end2] else [end2..start2]

intersectPerpendicular :: Segment2D -> Segment2D -> Maybe Point2D
intersectPerpendicular s1@((xStart1, yStart1), (xEnd1, _)) s2@((xStart2, yStart2), (_, yEnd2))
    | horizontal s1 = if
        | xStart1 <= xStart2 && xEnd1 >= xStart2 && yStart2 <= yStart1 && yEnd2 >= yStart1 -> Just (xStart2, yStart1)
        | xStart1 <= xStart2 && xEnd1 >= xStart2 && yStart2 >= yStart1 && yEnd2 <= yStart1 -> Just (xStart2, yStart1)
        | xStart1 >= xStart2 && xEnd1 <= xStart2 && yStart2 <= yStart1 && yEnd2 >= yStart1 -> Just (xStart2, yStart1)
        | xStart1 >= xStart2 && xEnd1 <= xStart2 && yStart2 >= yStart1 && yEnd2 <= yStart1 -> Just (xStart2, yStart1)
        | otherwise -> Nothing
    | horizontal s2 = intersectPerpendicular s2 s1
    | otherwise = undefined

intersect2D :: Segment2D -> Segment2D -> [Point2D]
intersect2D s1@((xStart1, yStart1), (xEnd1, yEnd1)) s2@((xStart2, yStart2), (xEnd2, yEnd2))
    | horizontal s1 && horizontal s2 = if
        | yStart1 == yStart2 -> (\x -> (x, yStart1)) <$> intersect1D (xStart1, xEnd1) (xStart2, xEnd2)
        | otherwise -> []
    | vertical s1 && vertical s2 = if
        | xStart1 == xStart2 -> (\y -> (xStart1, y)) <$> intersect1D (yStart1, yEnd1) (yStart2, yEnd2)
        | otherwise -> []
    | otherwise = maybeToList $ intersectPerpendicular s1 s2

readSegment2D :: String -> Segment2D
readSegment2D str = ((xStart, yStart), (xEnd, yEnd))
  where
    [startStr, _, endStr] = words str
    [xStart, yStart] = read <$> splitOn "," startStr :: [Int]
    [xEnd, yEnd] = read <$> splitOn "," endStr :: [Int]

countIntersections2D :: [Segment2D] -> Int
countIntersections2D segments = length intersectionPoints
  where
    intersectionPoints = nub $ concat [intersect2D s1 s2 | (s1:others) <- tails segments, s2 <- others]

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            allSegments <- map readSegment2D <$> lines <$> readFile filePath
            let validSegments = filter validSegment2D allSegments
            let numOfIntersections = countIntersections2D validSegments
            printf "Ans: %d\n" numOfIntersections
        _ -> error "Expected input file"
