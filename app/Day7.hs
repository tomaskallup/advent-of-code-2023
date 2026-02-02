module Day7 (run, compareHands2, getHandScore2) where

import Data.Bifunctor (bimap)
import Data.List
import Data.Ord

getInput :: Bool -> IO String
getInput True = readFile "inputs/day-7-example.txt"
getInput False = readFile "inputs/day-7.txt"

run :: Bool -> IO ()
run example = part1 example >> part2 example

charCounts :: String -> [(Char, Int)]
charCounts =
  sortOn (\(c, n) -> (Down n, Down $ getCardScore2 c)) . map (\x -> (head x, length x)) . group . sort

getHandScore :: (String, Int) -> Int
getHandScore (str, _) =
  case (map snd . charCounts) str of
    [5] -> 7
    4 : _ -> 6
    [3, 2] -> 5
    3 : _ -> 4
    [2, 2, 1] -> 3
    2 : _ -> 2
    _ -> 1

getCardScore :: Char -> Int
getCardScore 'A' = 13
getCardScore 'K' = 12
getCardScore 'Q' = 11
getCardScore 'J' = 10
getCardScore 'T' = 9
getCardScore ch = read [ch] - 1

compareHands :: (String, Int) -> (String, Int) -> Ordering
compareHands a b = if aScore /= bScore then compare aScore bScore else uncurry compare firstDiffScores
  where
    aScore = getHandScore a
    bScore = getHandScore b
    firstDiff = find (uncurry (/=)) $ zip (fst a) (fst b)
    firstDiffScores = maybe (1, 1) (Data.Bifunctor.bimap getCardScore getCardScore) firstDiff

parseLine :: String -> (String, Int)
parseLine str = (hand, read rest)
  where
    (hand, rst) = break (== ' ') str
    rest = dropWhile (== ' ') rst

part1 :: Bool -> IO ()
part1 example = do
  content <- getInput example
  let sorted = sortBy compareHands (map parseLine (lines content))
      result = zipWith (*) (map snd sorted) [1 ..]
  putStrLn ("Part 1: " ++ show (sum result))

getHandScore2 :: (String, Int) -> Int
getHandScore2 (str, _) =
  case charCounts str of
    [(_, 5)] -> 7
    [(_, _), ('J', _)] -> 7
    [('J', _), (_, _)] -> 7
    [(_, 4), (_, _)] -> 6
    [(_, 3), (_, 1), ('J', 1)] -> 6
    [(_, 2), ('J', 2), (_, 1)] -> 6
    [('J', 3), (_, 1), (_, 1)] -> 6
    [(_, 3), (_, 2)] -> 5
    [(_, 2), (_, 2), ('J', 1)] -> 5
    (_, 3) : _ -> 4
    [(_, 2), (_, 1), (_, 1), ('J', 1)] -> 4
    [('J', 2), (_, 1), (_, 1), (_, 1)] -> 4
    [(_, 2), (_, 2), (_, 1)] -> 3
    (_, 2) : _ -> 2
    [(_, 1), (_, 1), (_, 1), (_, 1), ('J', 1)] -> 2
    _ -> 1

getCardScore2 :: Char -> Int
getCardScore2 'A' = 13
getCardScore2 'K' = 12
getCardScore2 'Q' = 11
getCardScore2 'J' = 1
getCardScore2 'T' = 10
getCardScore2 ch = read [ch]

compareHands2 :: (String, Int) -> (String, Int) -> Ordering
compareHands2 a b = if aScore /= bScore then compare aScore bScore else uncurry compare firstDiffScores
  where
    aScore = getHandScore2 a
    bScore = getHandScore2 b
    firstDiff = find (uncurry (/=)) $ zip (fst a) (fst b)
    firstDiffScores = maybe (1, 1) (bimap getCardScore2 getCardScore2) firstDiff

part2 :: Bool -> IO ()
part2 example = do
  content <- getInput example
  let sorted = sortBy compareHands2 (map parseLine (lines content))
      result = zipWith (*) (map snd sorted) [1 ..]
  putStrLn ("Part 2: " ++ show (sum result))
