module Day1 (day1) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

day1 :: FilePath -> IO Int
day1 path = do
  file <- readFile path
  pure $ solveFirst file

solveFirst :: String -> Int
solveFirst s = sum $ map parseLine (lines s)

parseLine :: String -> Int
parseLine s = firstDigit s + 10 * secondDigit s

firstDigit :: String -> Int
firstDigit [] = error "impossible"
firstDigit s = if isDigit (last s) then digitToInt (last s) else firstDigit $ init s

secondDigit :: String -> Int
secondDigit [] = error "impossible"
secondDigit (c : s) = if isDigit c then digitToInt c else secondDigit s

digStrings :: [(Int, String)]
digStrings =
  [ (1, "one"),
    (2, "two"),
    (3, "three"),
    (4, "four"),
    (5, "five"),
    (6, "six"),
    (7, "seven"),
    (8, "eight"),
    (9, "nine")
  ]

parseLineWithStrings :: String -> Int
parseLineWithStrings s = undefined

parseFromLeft :: String -> Int
parseFromLeft [] = error "impossible"
parseFromLeft s = if snd (head digStrings) `isPrefixOf` s then 1 else parseFromLeft (tail s)

solveSecond :: String -> Int
solveSecond s = sum $ map parseLineWithStrings (lines s)
