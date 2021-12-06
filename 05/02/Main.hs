{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.List          (intersect, nub, tails)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (maybeToList)
import           System.Environment
import           Text.Printf

type Point2D = (Int, Int)
type Segment2D = (Point2D, Point2D)

horizontal :: Segment2D -> Bool
horizontal ((_, yStart), (_, yEnd)) = yStart == yEnd

vertical :: Segment2D -> Bool
vertical ((xStart, _), (xEnd, _)) = xStart == xEnd

diagonal :: Segment2D -> Bool
diagonal ((xStart, yStart), (xEnd, yEnd)) = abs (xEnd - xStart) == abs (yEnd - yStart)

validSegment2D :: Segment2D -> Bool
validSegment2D s = horizontal s || vertical s || diagonal s

pointsOf2D :: Segment2D -> [Point2D]
pointsOf2D s@((xStart, yStart), (xEnd, yEnd))
    | horizontal s = zip [xStart..xEnd] (repeat yStart)
    | vertical   s = zip (repeat xStart) [yStart..yEnd]
    | diagonal   s = zip [xStart..xEnd] (if yStart <= yEnd then [yStart..yEnd] else reverse [yEnd..yStart])
    | otherwise    = undefined

belongsTo2D :: Point2D -> Segment2D -> Bool
belongsTo2D (x, y) s@((xStart, yStart), (xEnd, yEnd))
    | horizontal s = y == yStart && x >= xStart && x <= xEnd
    | vertical   s = x == xStart && y >= yStart && y <= yEnd
    | diagonal   s && yStart <= yEnd = yStart - xStart == y - x && x >= xStart && x <= xEnd
    | diagonal   s && yStart >  yEnd = yStart + xStart == y + x && x >= xStart && x <= xEnd
    | otherwise    = undefined

intersectPerpendicular :: Segment2D -> Segment2D -> Maybe Point2D
intersectPerpendicular s1@((xStart1, yStart1), (xEnd1, yEnd1)) s2@((xStart2, yStart2), (xEnd2, yEnd2))
    | horizontal s1 && vertical s2 =
        if xStart1 <= xStart2 && xEnd1 >= xStart2 && yStart2 <= yStart1 && yEnd2 >= yStart1
        then Just (xStart2, yStart1)
        else Nothing
    | vertical s1 && horizontal s2 = intersectPerpendicular s2 s1
    | diagonal s1 && diagonal s2 && yStart1 > yEnd1 = intersectPerpendicular s2 s1
    | diagonal s1 && diagonal s2 =
        if b1 `mod` 2 == b2 `mod` 2 && belongsTo2D pDiag s1 && belongsTo2D pDiag s2
        then Just pDiag
        else Nothing
    | otherwise = undefined
  where
    b1 = yStart1 - xStart1
    b2 = yStart2 + xStart2
    pDiag = ((-b1 + b2) `div` 2, (b1 + b2) `div` 2)

intersect2D :: Segment2D -> Segment2D -> [Point2D]
intersect2D s1@((xStart1, yStart1), (xEnd1, yEnd1)) s2@((xStart2, yStart2), (xEnd2, yEnd2))
    | horizontal s1 && horizontal s2 = if
        | yStart1 == yStart2 -> intersect (pointsOf2D s1) (pointsOf2D s2)
        | otherwise          -> []
    | vertical s1 && vertical s2 = if
        | xStart1 == xStart2 -> intersect (pointsOf2D s1) (pointsOf2D s2)
        | otherwise          -> []
    | horizontal s1 && vertical s2 || vertical s1 && horizontal s2 = maybeToList $ intersectPerpendicular s1 s2
    | diagonal s1 && diagonal s2 = if
        | yStart1 <= yEnd1 && yStart2 >  yEnd2 -> maybeToList $ intersectPerpendicular s1 s2
        | yStart1 >  yEnd1 && yStart2 <= yEnd2 -> maybeToList $ intersectPerpendicular s1 s2
        | yStart1 <= yEnd1 && yStart2 <= yEnd2 && xStart1 - yStart1 == xStart2 - yStart2 -> intersect (pointsOf2D s1) (pointsOf2D s2)
        | yStart1 >  yEnd1 && yStart2 >  yEnd2 && xStart1 + yStart1 == xStart2 + yStart2 -> intersect (pointsOf2D s1) (pointsOf2D s2)
        | otherwise -> []
    | diagonal s2 = intersect2D s2 s1
    | diagonal s1 && yStart1 <= yEnd1 && horizontal s2 = let p = (-yStart1 + xStart1 + yStart2, yStart2) in if belongsTo2D p s1 && belongsTo2D p s2 then [p] else []
    | diagonal s1 && yStart1 >  yEnd1 && horizontal s2 = let p = ( yStart1 + xStart1 - yStart2, yStart2) in if belongsTo2D p s1 && belongsTo2D p s2 then [p] else []
    | diagonal s1 && yStart1 <= yEnd1 && vertical   s2 = let p = ( xStart2, yStart1 - xStart1 + xStart2) in if belongsTo2D p s1 && belongsTo2D p s2 then [p] else []
    | diagonal s1 && yStart1 >  yEnd1 && vertical   s2 = let p = ( xStart2, yStart1 + xStart1 - xStart2) in if belongsTo2D p s1 && belongsTo2D p s2 then [p] else []
    | otherwise = undefined

normalizeSegment2D :: Segment2D -> Segment2D
normalizeSegment2D (pStart@(xStart, yStart), pEnd@(xEnd, yEnd))
    | xStart == xEnd =
        if yStart <= yEnd
        then (pStart, pEnd)
        else (pEnd, pStart)
    | xStart <  xEnd = (pStart, pEnd)
    | xStart >  xEnd = (pEnd, pStart)

readSegment2D :: String -> Segment2D
readSegment2D str = ((xStart, yStart), (xEnd, yEnd))
  where
    [startStr, _, endStr] = words str
    [xStart, yStart] = read <$> splitOn "," startStr :: [Int]
    [xEnd, yEnd] = read <$> splitOn "," endStr :: [Int]

intersections2D :: [Segment2D] -> [Point2D]
intersections2D segments = nub $ concat [intersect2D s1 s2 | (s1:others) <- tails segments, s2 <- others]

countIntersections2D :: [Segment2D] -> Int
countIntersections2D = length . intersections2D

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            allSegments <- map (normalizeSegment2D . readSegment2D) <$> lines <$> readFile filePath
            let validSegments = filter validSegment2D allSegments
            printf "Num of segments: %d\n" $ length validSegments
            let numOfIntersections = countIntersections2D validSegments
            printf "Ans: %d\n" numOfIntersections
--            printf "Intersections:\n"
--            putStrLn . unwords $ (\(x, y) -> printf "(%d, %d)" x y) <$> intersections2D validSegments
        _ -> error "Expected input file"
