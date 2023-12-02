module Day1 (day1) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

day1 :: FilePath -> IO String
day1 path = do
  file <- readFile path
  pure $ show (solveFirst file) ++  " and " ++ show (solveSecond file)

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
parseLineWithStrings s = 10 * parseFromRight digStrings s +  parseFromLeft digStrings (reverse s)

parseFromLeft :: [(Int, String)] -> String -> Int
parseFromLeft _ [] = error "impossible"
parseFromLeft c s = if f c s > 0 || isDigit (head s) then (if isDigit (head s) then digitToInt (head s) else f c s) else parseFromLeft c (tail s)
  where
    f [] _ = 0
    f c' s' = if reverse (snd (head c')) `isPrefixOf` s' then fst (head c') else f (tail c') s'

parseFromRight :: [(Int, String)] -> String -> Int
parseFromRight _ [] = error "impossible"
parseFromRight c s = if f c s > 0 || isDigit (head s) then (if isDigit (head s) then digitToInt (head s) else f c s) else parseFromRight c (tail s)
  where
    f [] _ = 0
    f c' s' = if snd (head c') `isPrefixOf` s' then fst (head c') else f (tail c') s'

solveSecond :: String -> Int
solveSecond s = sum $ map parseLineWithStrings (lines s)