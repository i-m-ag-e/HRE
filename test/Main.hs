module Main (main) where

import MyLib( matchRE, searchRE )
import Test.HUnit

tests :: Test
tests = TestList
  [
    ----------------------------------------------------------------
    -- 1. Matches 
    ----------------------------------------------------------------
    TestLabel "Literal single char" $ TestCase $
      matchRE "a" "a" @?= Just True,

    TestLabel "Literal multi-char" $ TestCase $
      matchRE "abc" "abc" @?= Just True,

    TestLabel "Star matches empty" $ TestCase $
      matchRE "a*" "" @?= Just True,

    TestLabel "Star matches multiple" $ TestCase $
      matchRE "a*" "aaaa" @?= Just True,

    TestLabel "Plus requires at least one" $ TestCase $
      matchRE "a+" "aaa" @?= Just True,

    TestLabel "Question mark matches single" $ TestCase $
      matchRE "ab?" "ab" @?= Just True,

    TestLabel "Question mark matches empty" $ TestCase $
      matchRE "ab?" "a" @?= Just True,

    TestLabel "Grouping repetition" $ TestCase $
      matchRE "(ab)+" "ababab" @?= Just True,

    TestLabel "Alternation first branch" $ TestCase $
      matchRE "cat|dog" "cat" @?= Just True,

    TestLabel "Alternation second branch" $ TestCase $
      matchRE "cat|dog" "dog" @?= Just True,

    TestLabel "Nested alternation and grouping" $ TestCase $
      matchRE "(ab|cd)+" "abcdab" @?= Just True,

    TestLabel "Concatenation with star" $ TestCase $
      matchRE "ab*c" "abbbc" @?= Just True,

    TestLabel "Mix of plus and optional" $ TestCase $
      matchRE "a+b?" "aaab" @?= Just True,

    ----------------------------------------------------------------
    -- 2. Doesn't match 
    ----------------------------------------------------------------
    TestLabel "Literal mismatch" $ TestCase $
      matchRE "abc" "abd" @?= Just False,

    TestLabel "Star mismatch (different char)" $ TestCase $
      matchRE "a*" "bbb" @?= Just False,

    TestLabel "Plus requires at least one mismatch" $ TestCase $
      matchRE "a+" "" @?= Just False,

    TestLabel "Grouping mismatch" $ TestCase $
      matchRE "(ab)+" "aba" @?= Just False,

    TestLabel "Alternation mismatch" $ TestCase $
      matchRE "cat|dog" "cow" @?= Just False,

    ----------------------------------------------------------------
    -- 3. Substring search success
    ----------------------------------------------------------------
    TestLabel "Search literal inside word" $ TestCase $
      searchRE "cat" "concatenate" @?= Just True,

    TestLabel "Search group repetition inside" $ TestCase $
      searchRE "(ab)+" "zzababzz" @?= Just True,

    TestLabel "Search alternation inside" $ TestCase $
      searchRE "dog|cat" "hotdogstand" @?= Just True,

    ----------------------------------------------------------------
    -- 4. Substring search failure
    ----------------------------------------------------------------
    TestLabel "Search literal not present" $ TestCase $
      searchRE "cat" "doghouse" @?= Just False,

    TestLabel "Search group not present" $ TestCase $
      searchRE "(ab)+" "acacac" @?= Just False,

    TestLabel "Search alternation not present" $ TestCase $
      searchRE "dog|cat" "elephant" @?= Just False
  ]

main :: IO Counts
main = runTestTT tests
