{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}

module Main (main) where

import           Data.Either                   (fromRight)
import           Data.List                     (maximumBy)
import           Data.Ord                      (comparing)
import           System.Environment
import           Text.ParserCombinators.Parsec
import           Text.Printf

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

data SnailfishNumber = Value Int | Pair SnailfishNumber SnailfishNumber

instance Show SnailfishNumber where
    show (Value x)  = show x
    show (Pair l r) = "[" ++ show l ++ ", " ++ show r ++ "]"

magnitude :: SnailfishNumber -> Int
magnitude (Value x)  = x
magnitude (Pair l r) = 3 * (magnitude l) + 2 * (magnitude r)

snailfishNumber :: Parser SnailfishNumber
snailfishNumber = try pair <|> value

pair :: Parser SnailfishNumber
pair = do
    _ <- char '['
    nextCharL <- lookAhead anyChar
    l <- case nextCharL of
        '[' -> snailfishNumber
        _   -> value
    _ <- char ','
    nextCharR <- lookAhead anyChar
    r <- case nextCharR of
        '[' -> snailfishNumber
        _   -> value
    _ <- char ']'
    return $ Pair l r

value :: Parser SnailfishNumber
value = Value <$> read <$> many1 digit

reduceSnailfishNumber :: SnailfishNumber -> SnailfishNumber
reduceSnailfishNumber num = case tryExplode num of
    (newNum, True) -> reduceSnailfishNumber newNum
    (_, False) -> case trySplit num of
        (newNum, True) -> reduceSnailfishNumber newNum
        (_, False)     -> num

tryExplode :: SnailfishNumber -> (SnailfishNumber, Bool)
tryExplode x = case tryExplodeHelper 0 x of
    (_, Nothing)        -> (x, False)
    (newX, Just (_, _)) -> (newX, True)
  where
    tryExplodeHelper :: Int -> SnailfishNumber -> (SnailfishNumber, Maybe (Int, Int))
    tryExplodeHelper _ num@(Value _) = (num, Nothing)
    tryExplodeHelper depth num@(Pair (Value l) (Value r)) = if depth >= 4 then (Value 0, Just (l, r)) else (num, Nothing)
    tryExplodeHelper depth num@(Pair l r) = case tryExplodeHelper (depth + 1) l of
        (resL, Just (incL, incR)) -> (Pair resL (incLeftMost r incR), Just (incL, 0))
        (_, Nothing) -> case tryExplodeHelper (depth + 1) r of
            (resR, Just (incL, incR)) -> (Pair (incRightMost l incL) resR, Just (0, incR))
            (_, Nothing) -> (num, Nothing)

trySplit :: SnailfishNumber -> (SnailfishNumber, Bool)
trySplit num@(Value x) = if x >= 10 then (Pair (Value $ x `div` 2) (Value $ (x + 1) `div` 2), True) else (num, False)
trySplit num@(Pair l r) = case trySplit l of
    (newL, True) -> (Pair newL r, True)
    (_, False) -> case trySplit r of
        (newR, True) -> (Pair l newR, True)
        (_, False)   -> (num, False)

incLeftMost :: SnailfishNumber -> Int -> SnailfishNumber
incLeftMost num 0          = num
incLeftMost (Value x) inc  = Value $ x + inc
incLeftMost (Pair l r) inc = Pair (incLeftMost l inc) r

incRightMost :: SnailfishNumber -> Int -> SnailfishNumber
incRightMost num 0          = num
incRightMost (Value x) inc  = Value $ x + inc
incRightMost (Pair l r) inc = Pair l (incRightMost r inc)

addSnailfishNumbers :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
addSnailfishNumbers l r = reduceSnailfishNumber $ Pair l r

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            numbers <- map (fromRight (Value 0) . parse pair "snailfishNumber") . lines <$> readFile filePath :: IO [SnailfishNumber]
            let enumeratedNumbers = enumerate numbers
            let (resultX, resultY) = maximumBy (comparing $ uncurry (((magnitude . reduceSnailfishNumber) .) . addSnailfishNumbers)) [(x, y) | (i, x) <- enumeratedNumbers, (j, y) <- enumeratedNumbers, i /= j]
            printf "Ans:\n  X: %s\n  Y: %s\n" (show resultX) (show resultY)
            printf "Ans magnitude: %d\n" $ magnitude $ reduceSnailfishNumber $ addSnailfishNumbers resultX resultY
        _ -> error "Expected input file"
