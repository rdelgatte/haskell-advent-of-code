module Day5Spec where

import Day5 (seatId, valuesToInt)
import Test.Tasty.HUnit (testCase, (@?=))

test_valuesToInt =
  testCase "valuesToInt" $
    valuesToInt [0, 1, 0, 1, 1, 0, 0] @?= 44

test_seatId =
  testCase "seatId" $
    seatId "FBFBBFFRLR" @?= 357
