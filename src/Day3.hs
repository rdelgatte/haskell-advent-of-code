module Day3 where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Prelude

type Col = Int

type Row = Int

type Position = (Int, Int)

runDay3 :: IO ()
runDay3 = do
  print "--- Day 3: Toboggan Trajectory ---"
  print "Part 1: How many trees would you encounter?"
  result <- readFileContentToLine filepath
  print $ hits slope31 0 (Just (0, 0)) $ convert result
  print "Part 2: What do you get if you multiply together the number of trees encountered on each of the listed slopes?"
  let res = fmap (\slope -> hits slope 0 (Just (0, 0)) $ convert result) allSlopes
  print $ product res
  where
    slope31 = buildSlope 3 1
    filepath = "resources/day-3-input.txt"

readFileContentToLine :: String -> IO [Text.Text]
readFileContentToLine filePath = Text.lines <$> TextIO.readFile filePath

convert :: [Text.Text] -> [String]
convert = fmap Text.unpack

findChar :: Position -> [String] -> Maybe Char
findChar _ [] = Nothing
findChar (x, y) chars
  | x >= maxX || y >= maxY = Nothing
  | otherwise = Just $ chars !! y !! x
  where
    maxX = List.length (head chars)
    maxY = List.length chars

hits :: (Position -> Position) -> Int -> Maybe Position -> [String] -> Int
hits _ initialCount Nothing _ = initialCount
hits slope initialCount (Just (x, y)) lines =
  case findChar (x, y) lines of
    Nothing -> initialCount
    Just '#' -> hits slope (initialCount + 1) nextPosition lines
    Just _ -> hits slope initialCount nextPosition lines
  where
    nextPosition = nextCoordinate slope (x, y) lines

nextCoordinate :: (Position -> Position) -> Position -> [String] -> Maybe Position
nextCoordinate updatePosition (x, y) lines
  | nextY >= maxY = Nothing
  | nextX >= maxX = Just (nextX - maxX, nextY)
  | otherwise = Just (nextX, nextY)
  where
    (nextX, nextY) = updatePosition (x, y)
    maxY = length lines
    maxX = length (lines !! y)

buildSlope :: Int -> Int -> (Position -> Position)
buildSlope xx yy (x, y) = (x + xx, y + yy)

allSlopes :: [Position -> Position]
allSlopes = [buildSlope 1 1, buildSlope 3 1, buildSlope 5 1, buildSlope 7 1, buildSlope 1 2]
