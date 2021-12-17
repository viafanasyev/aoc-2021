{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}

module Main (main) where

import           Control.Applicative
import           Data.Char           (chr)
import           Data.List           (group, sort)
import           Data.List.Split     (splitOn, splitWhen)
import           System.Environment
import           Text.Printf

newtype Rule a = Rule { runRule :: (Char, Char) -> Either (Char, Char) (Char, a, Char) }

instance Functor Rule where
    fmap f (Rule rule) = Rule $ \input -> do
        (l, res, r) <- rule input
        return (l, f res, r)

instance Applicative Rule where
    pure x = Rule $ \(l, r) -> Right (l, x, r)
    (Rule r1) <*> (Rule r2) = Rule $ \input -> do
        (l, f, r) <- r1 input
        (l', res, r') <- r2 (l, r)
        return (l', f res, r')

instance Alternative (Either (Char, Char)) where
    empty = Left $ (chr 0, chr 0)
    Left _ <|> e2 = e2
    e1 <|> _      = e1

instance Alternative Rule where
    empty = Rule $ \(l, r) -> Left (l, r)
    (Rule r1) <|> (Rule r2) = Rule $ \input -> r1 input <|> r2 input

readRule :: String -> Rule Char
readRule s = Rule $ \(l, r) -> if l == fromL && r == fromR then Right (l, to, r) else Left (l, r)
  where
    [[fromL, fromR], [to]] = splitOn " -> " s

zipWithNext :: [a] -> [(a, a)]
zipWithNext xs = zip xs $ tail xs

unzipAppliedRules :: [Either (Char, Char) (Char, Char, Char)] -> String
unzipAppliedRules []                       = []
unzipAppliedRules [Left (l, r)]            = [l, r]
unzipAppliedRules [Right (l, res, r)]      = [l, res, r]
unzipAppliedRules ((Left (l, _)):xs)       = l : (unzipAppliedRules xs)
unzipAppliedRules ((Right (l, res, _)):xs) = l : res : (unzipAppliedRules xs)

applyRule :: Rule Char -> String -> String
applyRule r s = unzipAppliedRules $ map (runRule r) $ zipWithNext s

mostCommonCount :: Ord a => [a] -> Int
mostCommonCount = maximum . map length . group . sort

leastCommonCount :: Ord a => [a] -> Int
leastCommonCount = minimum . map length . group . sort

printResult :: String -> IO ()
printResult result = do
    let mostCommon = mostCommonCount result
    let leastCommon = leastCommonCount result
    printf "Result: %s\n" result
    printf "Most common: %d\n" mostCommon
    printf "Least common: %d\n" leastCommon


main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            rows <- lines <$> readFile filePath
            let [rawTemplate, rawRules] = splitWhen null rows
            let template = head rawTemplate
            let allRules = foldl1 (<|>) $ map readRule rawRules
            mapM_ printResult $ take 11 $ iterate (applyRule allRules) template
            --let result = (iterate (applyRule allRules) template) !! 10
            --let mostCommon = mostCommonCount result
            --let leastCommon = leastCommonCount result
            --printf "Result: %s\n" result
            --printf "Most common: %d\n" mostCommon
            --printf "Least common: %d\n" leastCommon
            --printf "Ans: %d\n" $ mostCommon - leastCommon
        _ -> error "Expected input file"
