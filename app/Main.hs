module Main where

import Day1
import Lib

main :: IO ()
main = do
  solday1 <- day1 "resources/day1.txt"
  print solday1
