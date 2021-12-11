{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.Maybe         (fromJust, isNothing)
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
bracketScore ')' = 3
bracketScore ']' = 57
bracketScore '}' = 1197
bracketScore '>' = 25137
bracketScore  _  = 0

firstIllegal :: String -> Maybe Char
firstIllegal = firstIllegalHelper []
  where
    firstIllegalHelper :: [Char] -> String -> Maybe Char
    firstIllegalHelper [] [] = Nothing
    firstIllegalHelper _  [] = Nothing
    firstIllegalHelper [] (b:bs)
        | isCloseBracket b = Just b
        | otherwise = firstIllegalHelper [b] bs
    firstIllegalHelper (lastOpened:opened) (b:bs)
        | isCloseBracket b = if isMatchingBrackets lastOpened b then firstIllegalHelper opened bs else Just b
        | otherwise = firstIllegalHelper (b : lastOpened : opened) bs

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            rows <- lines <$> readFile filePath
            let illegals = map firstIllegal rows
            let totalScore = sum $ map (\b -> if isNothing b then 0 else bracketScore $ fromJust b) illegals
            printf "Ans: %d\n" totalScore
        _ -> error "Expected input file"
