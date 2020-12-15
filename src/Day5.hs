{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day5 where

import Data.List
import Data.Text (Text (), unpack, words)
import qualified Data.Text.IO as TextIO
import Prelude

runDay5 :: IO ()
runDay5 = do
  print "--- Day 5: Binary Boarding ---"
  print "Part 1: What is the highest seat ID on a boarding pass?"
  result <- readFileContentToLine filepath
  print $ maximum $ fmap (seatId . unpack) result
  print "Part 2: What is the ID of your seat?"
  print $ findSeatId $ fmap (seatId . unpack) result
  where
    filepath = "resources/day-5-input.txt"

readFileContentToLine :: String -> IO [Text]
readFileContentToLine filePath = Data.Text.words <$> TextIO.readFile filePath

seatId :: String -> Int
seatId seat = seatRow * 8 + sitColumn
  where
    seatRow = seatValue $ take 7 seat
    sitColumn = seatValue $ drop 7 seat

seatValue :: String -> Int
seatValue s = valuesToInt $ fmap charToInt s
  where
    charToInt :: Char -> Int
    charToInt 'F' = 0
    charToInt 'B' = 1
    charToInt 'L' = 0
    charToInt 'R' = 1
    charToInt _ = error "Invalid character"

valuesToInt :: [Int] -> Int
valuesToInt values = foldl (\acc (key, _) -> 2 ^ (size - key) + acc) 0 keys
  where
    size = length values - 1
    keys = filter (\(_, a) -> a == 1) $ zip [0 ..] values

findSeatId :: [Int] -> Maybe Int
findSeatId values = find (`notElem` values) [min .. max]
  where
    min = minimum values
    max = maximum values
