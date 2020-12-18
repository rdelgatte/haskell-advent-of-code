{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Day6 where

import Data.List
import Data.Text (Text (), dropEnd, pack, splitOn, unpack)
import qualified Data.Text.IO as TextIO
import Prelude

runDay6 :: IO ()
runDay6 = do
  print "--- Day 6: Custom Customs ---"
  print "Part 1: What is the sum of those counts?"
  result <- readFileContentToLine filepath
  print $ countAnswers $ toString result
  print "Part 2: What is the sum of those counts?"
  print $ countCommonAnswers $ toString result
  where
    filepath = "resources/day-6-input.txt"

readFileContentToLine :: String -> IO [[Text]]
readFileContentToLine filePath = fmap (splitOn newLine) <$> (splitOn emptyNewLine . dropEnd 2 <$> TextIO.readFile filePath)
  where
    emptyNewLine = pack "\n\n"
    newLine = pack "\n"

toString :: [[Text]] -> [[String]]
toString = fmap (fmap unpack)

answered :: [String] -> String
answered values = nub $ concat values

countAnswers :: [[String]] -> Int
countAnswers values = sum $ fmap (length . answered) values

ordered :: [String] -> [String]
ordered = sortBy (\a b -> compare (length a) (length b))

allAnswered :: [String] -> String
allAnswered [] = ""
allAnswered [head] = head
allAnswered (head : tail) = filter (findChar tail) head

findChar :: [String] -> Char -> Bool
findChar values c = all (elem c) values

countCommonAnswers :: [[String]] -> Int
countCommonAnswers values = sum $ fmap (length . allAnswered) values
