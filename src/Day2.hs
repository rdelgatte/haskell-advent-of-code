{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day2 where

import Data.List
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Prelude

data Password = Password
  { first :: Int,
    second :: Int,
    char :: Char,
    password :: String
  }
  deriving (Show, Eq)

runDay2 :: IO ()
runDay2 = do
  print "--- Day 2: Password Philosophy---"
  print "Part 1: How many passwords are valid according to their policies?"
  result <- readFileContentToLine filepath
  print $ countValidPasswords result
  print "Part 2: How many passwords are valid according to the new interpretation of the policies?"
  result <- readFileContentToLine filepath
  print $ countValidPasswordsWithNewPolicy result
  where
    filepath = "resources/day-2-input.txt"

countValidPasswords :: [Text.Text] -> Int
countValidPasswords = length . filter isPasswordValid . fmap parseLine

parseLine :: Text.Text -> Maybe Password
parseLine value = parsePassword $ Text.unpack <$> Text.words cleanedUpValue
  where
    dash = Text.pack "-"
    colon = Text.pack ":"
    space = Text.pack " "
    -- Replace "-" with space and remove ":" character
    cleanedUpValue = Text.replace colon Text.empty $ Text.replace dash space value

parsePassword :: [String] -> Maybe Password
parsePassword (a : b : c : d : _) =
  Just $
    Password
      { first = read a,
        second = read b,
        char = head c,
        password = d
      }
parsePassword _ = Nothing

isPasswordValid :: Maybe Password -> Bool
isPasswordValid Nothing = False
isPasswordValid (Just Password {..}) = foundChars `elem` limits
  where
    foundChars :: Int = Text.count (Text.singleton char) (Text.pack password)
    limits :: [Int] = enumFromTo first second

readFileContentToLine :: String -> IO [Text.Text]
readFileContentToLine filePath = Text.lines <$> TextIO.readFile filePath

countValidPasswordsWithNewPolicy :: [Text.Text] -> Int
countValidPasswordsWithNewPolicy = length . filter isPasswordValidWithNewPolicy . fmap parseLine

isPasswordValidWithNewPolicy :: Maybe Password -> Bool
isPasswordValidWithNewPolicy Nothing = False
isPasswordValidWithNewPolicy (Just Password {..}) = (firstChar == char && secondChar /= char) || (firstChar /= char && secondChar == char)
  where
    firstChar = (!!) password (first - 1)
    secondChar = (!!) password (second - 1)
