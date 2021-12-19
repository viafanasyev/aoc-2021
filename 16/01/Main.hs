{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}

module Main (main) where

import           System.Environment
import           Text.ParserCombinators.Parsec
import           Text.Printf

data Packet
    = LiteralPacket Int Int -- version, value
    | OperatorPacket Int Int [Packet] -- version, typeId, subPackets
    deriving stock (Show, Eq)

versionsSum :: Packet -> Int
versionsSum (LiteralPacket v _) = v
versionsSum (OperatorPacket v _ subPackets) = v + (sum $ map versionsSum subPackets)

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"

binToDec :: String -> Int
binToDec = foldl (\acc c -> if c == '0' then acc * 2 else acc * 2 + 1) 0

packet :: Parser Packet
packet = try literalPacket <|> operatorPacket

literalPacket :: Parser Packet
literalPacket = do
    version <- packetVersion
    _ <- string "100"
    value <- binToDec <$> literalPacketBlocks
    return $ LiteralPacket version value

literalPacketBlocks :: Parser String
literalPacketBlocks = do
    hasNext <- binDigit
    blockValue <- count 4 binDigit
    case hasNext of
        '0' -> return blockValue
        '1' -> do
            otherBlocks <- literalPacketBlocks
            return $ blockValue ++ otherBlocks

operatorPacket :: Parser Packet
operatorPacket = do
    version <- packetVersion
    typeId <- binToDec <$> count 3 binDigit
    lenTypeId <- binDigit
    case lenTypeId of
        '0' -> do
            subPacketsBinLen <- binToDec <$> count 15 binDigit
            subPacketsData <- count subPacketsBinLen binDigit
            case parse (many packet) "subpackets" subPacketsData of
                Left _           -> fail "Error parsing subpackets"
                Right subPackets -> return $ OperatorPacket version typeId subPackets
        '1' -> do
            subPacketsNumber <- binToDec <$> count 11 binDigit
            subPackets <- count subPacketsNumber packet
            return $ OperatorPacket version typeId subPackets

packetVersion :: Parser Int
packetVersion = fmap binToDec $ count 3 binDigit

binDigit :: Parser Char
binDigit = oneOf "01"

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            printf "Input file: %s\n" filePath
            hexStr <- head . lines <$> readFile filePath
            let binStr = concat $ map hexToBin hexStr
            case parse packet "packet" binStr of
                Left err         -> printf "Parse error: %s\n" $ show err
                Right rootPacket -> printf "Ans: %d\n" $ versionsSum rootPacket
        _ -> error "Expected input file"
