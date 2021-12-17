{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}

module Main (main) where

import           Control.Applicative
import           Data.Char           (chr)
import           Data.List           (group, sort)
import           Data.List.Split     (splitOn, splitWhen)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           System.Environment
import           Text.Printf

type Result = Map (Char, Char) Int

readRule :: String -> Map (Char, Char) Result
readRule s = Map.singleton (fromL, fromR) (Map.fromListWith (+) [((fromL, to), 1), ((to, fromR), 1)])
  where
    [[fromL, fromR], [to]] = splitOn " -> " s

zipWithNext :: [a] -> [(a, a)]
zipWithNext xs = zip xs $ tail xs

nextIter :: Map (Char, Char) Result -> Map (Char, Char) Result -> Map (Char, Char) Result
nextIter initialRules rules = foldl (\resRules rule -> Map.insert rule (nextIterOne rule) resRules) Map.empty (Map.keys rules)
  where
    nextIterOne :: (Char, Char) -> Result
    nextIterOne rule = foldl1 addResults $ Map.mapWithKey (\subrule val -> mulResult val $ initialRules Map.! subrule) (rules Map.! rule)

mulResult :: Int -> Result -> Result
mulResult value = Map.map (*value)

divResult :: Int -> Result -> Result
divResult value = Map.map (`div` value)

addResults :: Result -> Result -> Result
addResults = Map.unionWith (+)

resultToLettersAppearances :: Result -> Map Char Int
resultToLettersAppearances result = Map.map (`div` 2) $ Map.unionWith (+) (Map.mapKeysWith (+) fst result) (Map.mapKeysWith (+) snd result)

countLettersAppearances :: String -> Map (Char, Char) Result -> Map Char Int
countLettersAppearances template result = resultToLettersAppearances pairsTotalNumber
  where
    pairs = zipWithNext template
    firstChar = head template
    lastChar = last template
    pairsTotalNumber = foldl1 addResults $ (Map.singleton (lastChar, firstChar) 1) : (map (result Map.!) pairs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            rows <- lines <$> readFile filePath
            let stepsNumber = 40
            let [rawTemplate, rawRules] = splitWhen null rows
            let template = head rawTemplate
            let rules = foldl1 Map.union $ map readRule rawRules
            let resultRules = (iterate (nextIter rules) rules) !! (stepsNumber - 1)
            let result = countLettersAppearances template resultRules
            printf "Ans: %d\n" $ (maximum $ Map.elems result) - (minimum $ Map.elems result)
        _ -> error "Expected input file"
