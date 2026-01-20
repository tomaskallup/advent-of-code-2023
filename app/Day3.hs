module Day3 (run) where

import Data.Char (isDigit)
import Data.List

getInput :: Bool -> IO String
getInput True = readFile "inputs/day-3-example.txt"
getInput False = readFile "inputs/day-3.txt"

run :: Bool -> IO ()
run example = part1 example >> part2 example

isValidChar :: Maybe Char -> Bool
isValidChar Nothing = False
isValidChar (Just '.') = False
isValidChar (Just ch) = not (isDigit ch)

_checkNum :: String -> (Int, Int) -> [String] -> Int
_checkNum digits (x, y) content
  | any isValidChar neighbours = read digits
  | otherwise = 0
  where
    neighbourPositions = (x - 1, y) : (x + length digits, y) : [(x2, y2) | x2 <- [x - 1 .. x + length digits], y2 <- [y - 1, y + 1]]
    neighbours = map (\(nX, nY) -> content !? nY >>= \row -> row !? nX) neighbourPositions

_checkLine :: String -> [String] -> Int -> Int -> Int
_checkLine [] _ _ acc = acc
_checkLine line content y acc = case digits of
  [] -> _checkLine (dropWhile (not . isDigit) line) content y acc
  _ -> _checkLine (drop (length digits) line) content y (acc + _checkNum digits (x, y) content)
  where
    digits = takeWhile isDigit line
    contentLine = case content of
      [] -> ""
      (row : _rest) -> row
    x = length contentLine - length line

checkLine :: String -> [String] -> Int -> Int
checkLine line content y = _checkLine line content y 0

_sumValidNumbers :: [String] -> [String] -> Int -> Int -> Int
_sumValidNumbers [] _ _y acc = acc
_sumValidNumbers (line : rest) content y acc = _sumValidNumbers rest content (y + 1) (acc + checkLine line content y)

sumValidNumbers :: [String] -> Int
sumValidNumbers content = _sumValidNumbers content content 0 0

part1 :: Bool -> IO ()
part1 example = do
  content <- getInput example
  let a = sumValidNumbers (lines content) in putStrLn ("Part 1: " ++ show a)

part2 :: Bool -> IO ()
part2 example = do
  content <- getInput example
  putStrLn ("Part 2: " ++ show content)
