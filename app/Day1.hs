module Day1 where

import Data.Char (isDigit)

getInput :: Bool -> Int -> IO String
getInput True 2 = readFile "inputs/day-1-example-2.txt"
getInput True _ = readFile "inputs/day-1-example.txt"
getInput False _ = readFile "inputs/day-1.txt"

run :: Bool -> IO ()
run example = part1 example >> part2 example

part1 :: Bool -> IO ()
part1 example = do
  content <- getInput example 1
  let result = _parseLines (lines content) 0
  putStrLn ("Part 1: " ++ show result)

_parseLines :: [String] -> Int -> Int
_parseLines rest acc =
  foldl (\acc2 line -> acc2 + _parseLine line) acc rest

_digitToInt :: (Enum a) => a -> Int
_digitToInt char = fromEnum char - fromEnum '0'

_parseLine :: String -> Int
_parseLine line = do
  let digits = filter isDigit line
   in case digits of
        [] -> 0
        [hd] -> _digitToInt hd * 10 + _digitToInt hd
        (hd : rest) -> _digitToInt hd * 10 + _digitToInt (last rest)

part2 :: Bool -> IO ()
part2 example = do
  content <- getInput example 2
  let result = _parseLines2 (lines content) 0
  putStrLn ("Part 2: " ++ show result)

_parseLines2 :: [String] -> Int -> Int
_parseLines2 rest acc =
  foldl (\acc2 line -> acc2 + _parseLine2 line) acc rest

_parseDigits :: String -> [Int]
_parseDigits "" = []
_parseDigits str@(c : cs)
  | isDigit c = read [c] : _parseDigits cs
  | take 3 str == "one" = 1 : _parseDigits (drop 2 str)
  | take 3 str == "two" = 2 : _parseDigits (drop 2 str)
  | take 5 str == "three" = 3 : _parseDigits (drop 4 str)
  | take 4 str == "four" = 4 : _parseDigits (drop 4 str)
  | take 4 str == "five" = 5 : _parseDigits (drop 3 str)
  | take 3 str == "six" = 6 : _parseDigits (drop 3 str)
  | take 5 str == "seven" = 7 : _parseDigits (drop 4 str)
  | take 5 str == "eight" = 8 : _parseDigits (drop 4 str)
  | take 4 str == "nine" = 9 : _parseDigits (drop 4 str)
  | otherwise = _parseDigits cs

_parseLine2 :: String -> Int
_parseLine2 line =
  let digits = _parseDigits line
   in case digits of
        [] -> 0
        [hd] -> hd * 10 + hd -- Handle single digit case
        (hd : rest) -> hd * 10 + last rest
