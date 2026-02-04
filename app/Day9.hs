{-# OPTIONS_GHC -Wno-x-partial #-}

module Day9 (run, splitOn) where

import Debug.Trace (trace)

getInput :: Bool -> IO String
getInput True = readFile "inputs/day-9-example.txt"
getInput False = readFile "inputs/day-9.txt"

run :: Bool -> IO ()
run example = part1 example >> part2 example

splitOn :: (Char -> Bool) -> String -> [String]
splitOn _ "" = []
splitOn sep str = first : splitOn sep (drop 1 rest)
  where
    (first, rest) = break sep str

advance :: [Int] -> [Int]
advance (first : second : rest) = (second - first) : advance (second : rest)
advance _ = []

advanceUntil :: ([Int] -> Bool) -> [Int] -> [[Int]]
advanceUntil check input
  | check input = [input]
  | otherwise = input : advanceUntil check (advance input)

getNextPrediction :: [[Int]] -> Int
getNextPrediction = foldl reducer 0 . reverse
  where
    reducer :: Int -> [Int] -> Int
    reducer acc v = acc + last v

getPrevPrediction :: [[Int]] -> Int
getPrevPrediction = foldl reducer 0 . reverse
  where
    reducer :: Int -> [Int] -> Int
    reducer acc v = head v - acc

_parseLine :: String -> [Int]
_parseLine l = map (read @Int) numbers
  where
    numbers = splitOn (== ' ') l

part1 :: Bool -> IO ()
part1 example = do
  content <- getInput example
  let inputs = map _parseLine $ lines content
      advanced = map (advanceUntil (all (== 0))) inputs
  putStrLn ("Part 1: " ++ show (sum $ map getNextPrediction advanced))

part2 :: Bool -> IO ()
part2 example = do
  content <- getInput example
  let inputs = map _parseLine $ lines content
      advanced = map (advanceUntil (all (== 0))) inputs
  putStrLn ("Part 2: " ++ show (sum $ map getPrevPrediction advanced))
