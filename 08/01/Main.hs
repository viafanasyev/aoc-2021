{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import           Data.List          (find, intersect, sort, union)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromJust)
import           System.Environment
import           Text.Printf

-- 1. |1| = 2
-- 7. |7| = 3
-- 4. |4| = 4
-- 8. |8| = 7

-- 6. |6| = 6, |6 `union` 1| = 7
-- 9. |9| = 6, |9 `union` 4| = 6

-- 2. |2| = 5, |2 `union` 4| = 7

-- 3. |3| = 5, |3 `intersect` 2| = 4
-- 5. |5| = 5, |5 `intersect` 2| = 3

-- 0. |0| = 6, |0 `union` 5| = 7

decode :: [String] -> [(String, Int)]
decode codes = [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9)]
  where
    one   = sort $ fromJust $ find ((==2) . length) codes
    seven = sort $ fromJust $ find ((==3) . length) codes
    four  = sort $ fromJust $ find ((==4) . length) codes
    eight = sort $ fromJust $ find ((==7) . length) codes
    six   = sort $ fromJust $ find (\x -> length x == 6 && length (x `union` one) == 7) codes
    nine  = sort $ fromJust $ find (\x -> length x == 6 && length (x `union` four) == 6) codes
    two   = sort $ fromJust $ find (\x -> length x == 5 && length (x `union` four) == 7) codes
    three = sort $ fromJust $ find (\x -> length x == 5 && length (x `intersect` two) == 4) codes
    five  = sort $ fromJust $ find (\x -> length x == 5 && length (x `intersect` two) == 3) codes
    zero  = sort $ fromJust $ find (\x -> length x == 6 && length (x `union` five) == 7) codes

readInputOutput :: String -> ([String], [String])
readInputOutput str = (input, output)
  where
    [input, output] = words <$> splitOn "|" str

decodeTest :: ([String], [String]) -> [Int]
decodeTest (input, output) = (\o -> fromJust $ lookup (sort o) codes) <$> output
  where
    codes = decode input

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            decodedTests <- map (decodeTest . readInputOutput) . lines <$> readFile filePath
            let ans = sum $ (length . filter (flip elem [1, 4, 7, 8])) <$> decodedTests
            printf "Ans: %d\n" ans
        _ -> error "Expected input file"
