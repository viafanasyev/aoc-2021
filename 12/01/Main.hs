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
countPaths f t g = countPathsHelper f t g []
  where
    countPathsHelper :: String -> String -> Map String [String] -> [String] -> Int
    countPathsHelper from to graph visited
        | from == to = 1
        | otherwise = case Map.lookup from graph of
            Nothing -> 0
            Just neighbours -> foldl (\res n -> if n `elem` visited then res else res + (countPathsHelper n to graph (if all isLower from then from : visited else visited))) 0 neighbours

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            graph <- readGraph . lines <$> readFile filePath
            printf "Ans: %d\n" $ countPaths "start" "end" graph
        _ -> error "Expected input file"
