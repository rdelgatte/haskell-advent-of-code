module Main where

import Day1

main :: IO ()
main = do
  result <- findTwoEntries filepath searched
  print result -- Result part 1 = Just 1009899
  result2 <- findResult filepath searched
  print result2 -- Result part 2 = Just 44211152
  where
    filepath = "resources/day-1-input.txt"
    searched = 2020
