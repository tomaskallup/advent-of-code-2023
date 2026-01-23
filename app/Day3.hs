module Day3 (run, checkMultiplications, extractLine) where

import Data.Char (isDigit)
import Data.List
import Data.Maybe (fromJust)

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
  putStrLn ("Part 1: " ++ show (sumValidNumbers (lines content)))

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0 ..]

countNumberCells :: (Bool, Bool, Bool) -> Int
countNumberCells input = case input of
  (False, False, False) -> 0
  (True, False, True) -> 2
  _ -> 1

extractLine :: (Bool, Bool, Bool) -> Int -> String -> Int
extractLine (True, True, True) x targetLine = read (left ++ right)
  where
    (leftPart, rightPart) = splitAt x targetLine
    left = reverse $ takeWhile isDigit (reverse leftPart)
    right = takeWhile isDigit rightPart
extractLine (True, True, False) x targetLine = read $ reverse (takeWhile isDigit (reverse (take (x + 1) targetLine)))
extractLine (True, False, False) x targetLine = read $ reverse (takeWhile isDigit (reverse (take x targetLine)))
extractLine (False, True, True) x targetLine = read $ takeWhile isDigit (drop x targetLine)
extractLine (False, False, True) x targetLine = read $ takeWhile isDigit (drop (x + 1) targetLine)
extractLine (False, True, False) x targetLine = read [targetLine !! x]
extractLine (True, False, True) x targetLine = read left * read right
  where
    (leftPart, rightPart) = splitAt x targetLine
    left = reverse $ takeWhile isDigit (reverse leftPart)
    right = takeWhile isDigit (drop 1 rightPart)
extractLine _ _ _ = 0

_extractNumberR :: String -> Int
_extractNumberR content = read (reverse $ takeWhile isDigit content)

_extractNumber :: String -> Int
_extractNumber content = read (takeWhile isDigit content)

_checkCellMultiplications :: (Maybe String, String, Maybe String) -> (Int, Char) -> Int -> Int
_checkCellMultiplications (prevLine, line, nextLine) (x, '*') acc = case numberNeighbourCounts of
  [2, 0, 0] -> acc + extractLine prevNeighbours x (fromJust prevLine)
  [0, 2, 0] -> acc + extractLine lineNeighbours x line
  [0, 0, 2] -> acc + extractLine nextNeighbours x (fromJust nextLine)
  [1, 0, 1] -> acc + extractLine prevNeighbours x (fromJust prevLine) * extractLine nextNeighbours x (fromJust nextLine)
  [1, 1, 0] -> acc + extractLine prevNeighbours x (fromJust prevLine) * extractLine lineNeighbours x line
  [0, 1, 1] -> acc + extractLine nextNeighbours x (fromJust nextLine) * extractLine lineNeighbours x line
  _ -> acc
  where
    getNeighbours targetLine = case targetLine of
      Nothing -> (False, False, False)
      Just l -> (maybe False isDigit (l !? (x - 1)), isDigit (l !! x), maybe False isDigit (l !? (x + 1)))
    prevNeighbours = getNeighbours prevLine
    nextNeighbours = getNeighbours nextLine
    lineNeighbours = getNeighbours $ Just line
    numberNeighbourCounts = [countNumberCells prevNeighbours, countNumberCells lineNeighbours, countNumberCells nextNeighbours]
_checkCellMultiplications _ _ acc = acc

_checkLineMultiplications :: (Maybe String, String, Maybe String) -> Int -> Int
_checkLineMultiplications contentLines@(_, line, _) acc = foldl f acc (withIndex line)
  where
    f = flip (_checkCellMultiplications contentLines)

checkMultiplications :: [String] -> Int
checkMultiplications content = foldl f 0 (withIndex content)
  where
    f acc (index, line) = _checkLineMultiplications (content !? (index - 1), line, content !? (index + 1)) acc

part2 :: Bool -> IO ()
part2 example = do
  content <- getInput example
  putStrLn ("Part 2: " ++ show (checkMultiplications $ lines content))
