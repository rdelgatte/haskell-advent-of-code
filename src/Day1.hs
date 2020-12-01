{-# LANGUAGE ScopedTypeVariables #-}

module Day1 where

import Data.List
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Text.Read as Read
import Prelude

readFileContentToLine :: String -> IO [Integer]
readFileContentToLine filePath = do
  (contentLines :: [Text.Text]) <- Text.lines <$> TextIO.readFile filePath
  return . sort $ mapMaybe readInteger contentLines

readInteger :: Text.Text -> Maybe Integer
readInteger value = case Read.decimal value of
  Right (result, _) -> Just result
  _ -> Nothing

filterAndSort :: Integer -> [Integer] -> [Integer]
filterAndSort limit = sort . filter (limit >=)

findFirstEntry :: Integer -> [Integer] -> Maybe Integer
findFirstEntry searched values = fst <$> find (\(value, others) -> (searched - value) `elem` others) zippedList
  where
    zippedList :: [(Integer, [Integer])] = zip values $ map (`delete` values) values

findTwoEntries :: String -> Integer -> IO (Maybe Integer)
findTwoEntries filePath searched = do
  values <- readFileContentToLine filePath
  let filteredAndSortedValues = filterAndSort searched values
      maybeFirstEntry = findFirstEntry searched filteredAndSortedValues
  return $ (\entry -> (searched - entry) * entry) <$> maybeFirstEntry

findThreeEntries :: Integer -> [Integer] -> Maybe (Integer, Integer)
findThreeEntries searched values = case result of
  [] -> Nothing
  res -> head res
  where
    filteredAndSortedValues = filterAndSort searched values
    result :: [Maybe (Integer, Integer)] =
      fmap
        ( \first -> case findFirstEntry (searched - first) filteredAndSortedValues of
            Just second -> Just (first, second)
            Nothing -> Nothing
        )
        filteredAndSortedValues

findResult :: String -> Integer -> IO (Maybe Integer)
findResult filePath searched = do
  values <- readFileContentToLine filePath
  return $ timesNumbers searched <$> findThreeEntries searched values

timesNumbers :: Integer -> (Integer, Integer) -> Integer
timesNumbers limit (first, second) = first * second * third
  where
    third = limit - (first + second)
