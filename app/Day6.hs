module Day6 (run) where

import Data.Char (isDigit)

getInput :: Bool -> IO String
getInput True = readFile "inputs/day-6-example.txt"
getInput False = readFile "inputs/day-6.txt"

run :: Bool -> IO ()
run example = part1 example >> part2 example

_parseLine :: String -> [Int]
_parseLine "" = []
_parseLine str = read first : _parseLine rest
  where
    start = dropWhile (not . isDigit) str
    (first, rest) = span isDigit start

parseContent :: String -> [(Int, Int)]
parseContent content = zip (_parseLine firstLine) (_parseLine rest)
  where
    (firstLine, rest) = break (== '\n') content

-- Find min & max x: x * (y-x) > z
getMinMax :: Int -> Int -> (Int, Int)
getMinMax y z =
  let yf = (fromIntegral @Int @Double) y
      zf = (fromIntegral @Int @Double) z
      discriminant = yf * yf - 4 * zf
      sqrtDisc = sqrt discriminant
      root1 = (yf - sqrtDisc) / 2
      root2 = (yf + sqrtDisc) / 2
      rawMinX = ceiling root1
      rawMaxX = floor root2
      minX = if rawMinX * (y - rawMinX) > z then rawMinX else rawMinX + 1
      maxX = if rawMaxX * (y - rawMaxX) > z then rawMaxX else rawMaxX - 1
   in (minX, maxX)

getCount :: [(Int, Int)] -> Int
getCount counts = product $ map f counts
  where
    f (y, z) = 1 + maxX - minX
      where
        (minX, maxX) = getMinMax y z

part1 :: Bool -> IO ()
part1 example = do
  content <- getInput example
  putStrLn ("Part 1: " ++ show (getCount $ parseContent content))

_parseLine2 :: String -> Int
_parseLine2 str = read $ filter isDigit str

parseContent2 :: String -> (Int, Int)
parseContent2 content = (_parseLine2 firstLine, _parseLine2 rest)
  where
    (firstLine, rest) = break (== '\n') content

part2 :: Bool -> IO ()
part2 example = do
  content <- getInput example
  putStrLn ("Part 2: " ++ show (getCount [parseContent2 content]))
