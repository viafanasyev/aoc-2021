{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.List          (sort)
import           System.Environment
import           Text.Printf

isOpenBracket :: Char -> Bool
isOpenBracket '(' = True
isOpenBracket '[' = True
isOpenBracket '{' = True
isOpenBracket '<' = True
isOpenBracket  _  = False

isCloseBracket :: Char -> Bool
isCloseBracket ')' = True
isCloseBracket ']' = True
isCloseBracket '}' = True
isCloseBracket '>' = True
isCloseBracket  _  = False

isMatchingBrackets :: Char -> Char -> Bool
isMatchingBrackets '(' ')' = True
isMatchingBrackets '[' ']' = True
isMatchingBrackets '{' '}' = True
isMatchingBrackets '<' '>' = True
isMatchingBrackets ')' '(' = True
isMatchingBrackets ']' '[' = True
isMatchingBrackets '}' '{' = True
isMatchingBrackets '>' '<' = True
isMatchingBrackets  _   _  = False

bracketScore :: Char -> Int
bracketScore '(' = 1
bracketScore '[' = 2
bracketScore '{' = 3
bracketScore '<' = 4
bracketScore  _  = 0

unclosed :: String -> [Char]
unclosed = unclosedHelper []
  where
    unclosedHelper :: [Char] -> String -> [Char]
    unclosedHelper [] [] = []
    unclosedHelper opened [] = opened
    unclosedHelper [] (b:bs)
        | isCloseBracket b = []
        | otherwise = unclosedHelper [b] bs
    unclosedHelper (lastOpened:opened) (b:bs)
        | isCloseBracket b = if isMatchingBrackets lastOpened b then unclosedHelper opened bs else []
        | otherwise = unclosedHelper (b : lastOpened : opened) bs

middle :: [a] -> a
middle [] = error "Empty list"
middle xs = xs !! midIndex
  where
    midIndex = length xs `div` 2


main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            rows <- lines <$> readFile filePath
            let uncloseds = filter (not . null) $ map unclosed rows
            let scores = map (foldl (\acc b -> acc * 5 + bracketScore b) 0) uncloseds
            let middleScore = middle $ sort scores
            printf "Num of incomplete: %d\n" $ length scores
            printf "Ans: %d\n" middleScore
        _ -> error "Expected input file"
