module Day5 (run) where

import Data.Char (isDigit)
import Data.List (find, sortOn)
import Data.Maybe

getInput :: Bool -> IO String
getInput True = readFile "inputs/day-5-example.txt"
getInput False = readFile "inputs/day-5.txt"

run :: Bool -> IO ()
run example = part1 example >> part2 example

data Transformation = Transformation
  { dest :: Int,
    source :: Int,
    len :: Int
  }
  deriving (Show)

_parseTransformation :: [Transformation] -> String -> [Transformation]
_parseTransformation acc line = case numbers of
  [dest, source, len] -> Transformation {dest, source, len} : acc
  _ -> acc
  where
    numbers = map (read @Int) $ words line

parseTransformationSection :: [String] -> ([Transformation], [String])
parseTransformationSection content = (sortOn source $ foldl _parseTransformation [] taken, drop 2 rest)
  where
    (taken, rest) = span (maybe False isDigit . listToMaybe) content

_parseContent :: [String] -> [[Transformation]] -> ([[Transformation]], [Int])
_parseContent content acc
  | isDigit (head line) = let (transformationSection, rest) = parseTransformationSection content in _parseContent rest (transformationSection : acc)
  | otherwise = (acc, map (read @Int) $ words $ drop 7 line)
  where
    line = head content

parseContent :: [String] -> ([[Transformation]], [Int])
parseContent content = _parseContent content []

_runTransformation :: Int -> Transformation -> Int
_runTransformation num Transformation {source, dest} = num + dest - source

convert :: [Int] -> [Transformation] -> [Int]
convert input transformations = map convertInput input
  where
    convertInput num = maybe num (_runTransformation num) transformation
      where
        matchTransformation Transformation {source, len} = num >= source && num < (source + len)
        transformation = find matchTransformation transformations

part1 :: Bool -> IO ()
part1 example = do
  content <- getInput example
  let (transformations, seeds) = parseContent $ reverse $ lines content
  putStrLn ("Part 1: " ++ show (minimum (foldl convert seeds transformations)))

_runRangeTransformation :: (Int, Int) -> Transformation -> ([(Int, Int)], [(Int, Int)])
_runRangeTransformation (start, size) Transformation {source, len, dest}
  | start + size <= source || start >= source + len =
      -- No overlap
      ([], [(start, size)])
  | start < source && start + size > source + len =
      -- Input range completely covers the transformation range
      let leftPart = (start, source - start)
          middlePart = (dest + (source - source), len)  -- Transform the overlapping part
          rightPart = (source + len, start + size - (source + len))
      in ([middlePart], [leftPart, rightPart])
  | start < source =
      -- Input starts before transformation but overlaps
      let leftPart = (start, source - start)
          overlapSize = start + size - source
          transformedPart = (dest, overlapSize)
      in ([transformedPart], [leftPart])
  | start + size > source + len =
      -- Input starts within transformation but extends beyond
      let overlapSize = source + len - start
          transformedPart = (dest + (start - source), overlapSize)
          rightPart = (source + len, start + size - (source + len))
      in ([transformedPart], [rightPart])
  | otherwise =
      -- Input is completely within transformation
      let adjustedStart = dest + (start - source)
      in ([(adjustedStart, size)], [])

convertSeedRange :: [(Int, Int)] -> [Transformation] -> [(Int, Int)]
convertSeedRange ranges transformations = concatMap (applyTransformsToRange transformations) ranges
  where
    applyTransformsToRange [] range = [range]
    applyTransformsToRange (t:ts) range =
      let (transformed, untransformed) = _runRangeTransformation range t
      in if not (null transformed)
           then transformed ++ concatMap (applyTransformsToRange ts) untransformed
           else applyTransformsToRange ts range

parseSeedRanges :: [Int] -> [(Int, Int)] -> [(Int, Int)]
parseSeedRanges (start : size : rest) acc = parseSeedRanges rest (acc ++ [(start, size)])
parseSeedRanges _ acc = acc

part2 :: Bool -> IO ()
part2 example = do
  content <- getInput example
  let (transformations, rawSeeds) = parseContent $ reverse $ lines content
  let seeds = parseSeedRanges rawSeeds []
  putStrLn ("Part 2: " ++ show (minimum $ map fst (foldl convertSeedRange seeds transformations)))
