{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.Char          (isLower)
import           Data.List.Split    (splitOn)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           System.Environment
import           Text.Printf

addEdge :: String -> String -> Map String [String] -> Map String [String]
addEdge from to graph = Map.insertWith (++) to [from] $ Map.insertWith (++) from [to] graph

readGraph :: [String] -> Map String [String]
readGraph = foldl (\graph [from, to] -> addEdge from to graph) Map.empty . map (splitOn "-")

countPaths :: String -> String -> Map String [String] -> Int
countPaths f t g = countPathsHelper f t g [] False
  where
    countPathsHelper :: String -> String -> Map String [String] -> [String] -> Bool -> Int
    countPathsHelper from to graph visited visitedSmallTwice
        | from == to = 1
        | otherwise = case Map.lookup from graph of
            Nothing -> 0
            Just neighbours -> foldl (\res n -> let newVisited = (if all isLower from then from : visited else visited) in if n `elem` visited then (if visitedSmallTwice || n == "start" || n == "end" then res else res + (countPathsHelper n to graph newVisited True)) else res + (countPathsHelper n to graph newVisited visitedSmallTwice)) 0 neighbours

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            graph <- readGraph . lines <$> readFile filePath
            printf "Ans: %d\n" $ countPaths "start" "end" graph
        _ -> error "Expected input file"
