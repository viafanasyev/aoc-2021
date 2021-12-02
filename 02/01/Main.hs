module Main where

import System.Environment
import Text.Printf

toDelta :: String -> (Int, Int)
toDelta ('f':'o':'r':'w':'a':'r':'d':' ':d) = (read d :: Int, 0)
toDelta ('d':'o':'w':'n':' ':d) = (0, read d :: Int)
toDelta ('u':'p':' ':d) = (0, -(read d :: Int))
toDelta _ = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath:_) -> do
      printf "Input file: %s\n" filePath
      deltas <- map toDelta . lines <$> readFile filePath
      let (x, y) = foldl (\(x, y) (xd, yd) -> (x + xd, y + yd)) (0, 0) deltas
      printf "X: %d, Y: %d\n" x y
      printf "Ans: %d\n" $ x * y
    _ -> error "Expected input file"
