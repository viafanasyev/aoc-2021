{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}

module Main (main) where

import           Data.Char          (digitToInt, isDigit)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           System.Environment
import           Text.Printf

data Env = Env
    { getInput :: [Int]
    , vars     :: Map Var Int
    }

type Var = String

getVar :: Env -> Var -> Int
getVar (Env _ vars) var = if isInt then read var else Map.findWithDefault 0 var vars
  where
    isInt = all isDigit var || (head var == '-' && all isDigit (tail var))

putVar :: Env -> Var -> Int -> Env
putVar (Env i vars) var value = Env i (Map.insert var value vars)

inputNext :: Env -> Var -> Env
inputNext (Env input vars) var = (Env (drop 1 input) (Map.insert var (head input) vars))

data Command
    = Input Var
    | Add Var Var
    | Mul Var Var
    | Div Var Var
    | Mod Var Var
    | Eql Var Var

eval :: Env -> Command -> Env
eval env (Input var) = inputNext env var
eval env (Add lhs rhs) =
    let lhsValue = getVar env lhs
        rhsValue = getVar env rhs
    in putVar env lhs (lhsValue + rhsValue)
eval env (Mul lhs rhs) =
    let lhsValue = getVar env lhs
        rhsValue = getVar env rhs
    in putVar env lhs (lhsValue * rhsValue)
eval env (Div lhs rhs) =
    let lhsValue = getVar env lhs
        rhsValue = getVar env rhs
    in putVar env lhs (lhsValue `div` rhsValue)
eval env (Mod lhs rhs) =
    let lhsValue = getVar env lhs
        rhsValue = getVar env rhs
    in putVar env lhs (lhsValue `mod` rhsValue)
eval env (Eql lhs rhs) =
    let lhsValue = getVar env lhs
        rhsValue = getVar env rhs
    in putVar env lhs (if lhsValue == rhsValue then 1 else 0)

readCommand :: String -> Command
readCommand str = case commandName of
    "inp" -> Input $ splittedStr !! 1
    "add" -> Add (splittedStr !! 1) (splittedStr !! 2)
    "mul" -> Mul (splittedStr !! 1) (splittedStr !! 2)
    "div" -> Div (splittedStr !! 1) (splittedStr !! 2)
    "mod" -> Mod (splittedStr !! 1) (splittedStr !! 2)
    "eql" -> Eql (splittedStr !! 1) (splittedStr !! 2)
  where
    splittedStr = words str
    commandName = head splittedStr

doesMonadAccept :: [Command] -> [Int] -> Bool
doesMonadAccept commands input = 0 == getVar resultEnv "z"
  where
    resultEnv = foldl eval (Env input Map.empty) commands

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            commands <- map readCommand . lines <$> readFile filePath
            let monad = doesMonadAccept commands
            let maxX = 39924989499969 :: Int
            let minX = 16811412161117 :: Int
            printf "Max: %d\nResult: %s\n" maxX $ if monad (map digitToInt $ show maxX) then "YES" else "NO"
            printf "Min: %d\nResult: %s\n" minX $ if monad (map digitToInt $ show minX) then "YES" else "NO"
        _ -> error "Expected input file"
