module Day6Spec where

import Day6
import Test.Tasty.HUnit (testCase, (@?=))

test_answered =
  testCase "get answered questions" $
    answered ["qpicundo", "fiqcdbkyuoz"] @?= "qpicundofbkyz"

test_answered_aaa =
  testCase "get answered questions for a-a-a-a" $
    answered ["a", "a", "a", "a"] @?= "a"

test_answered_abc =
  testCase "get answered questions for a-b-c" $
    answered ["a", "b", "c"] @?= "abc"

test_ordered =
  testCase "get ordered answers" $
    ordered ["aaaaa", "aaa", "bbbbb", ""] @?= ["", "aaa", "aaaaa", "bbbbb"]

test_allAnswered =
  testCase "get allAnswered answers" $
    allAnswered ["fcelpwgamhnquzbsrtdxivjk", "tdjwzsaqhxunkfcvpbrmgil"] @?= "fclpwgamhnquzbsrtdxivjk"

test_allAnswered_0 =
  testCase "get allAnswered 0" $
    allAnswered ["a", "b", "c"] @?= ""

test_allAnswered_1 =
  testCase "get allAnswered 1" $
    allAnswered ["ab", "ac"] @?= "a"

test_allAnswered_singleElement =
  testCase "get allAnswered single" $
    allAnswered ["a"] @?= "a"

test_allAnswered_empty =
  testCase "get allAnswered single" $
    length "fclpwgamhnquzbsrtdxivjk" @?= 23
