module Day4 (run) where

getInput :: Bool -> IO String
getInput True = readFile "inputs/day-4-example.txt"
getInput False = readFile "inputs/day-4.txt"

run :: Bool -> IO ()
run example = part1 example >> part2 example

_countWinners :: String -> Int
_countWinners line = length winningPulls
  where
    -- Drop "Card 123: "
    start = drop 2 $ dropWhile (/= ':') line
    (_winners, _pulls) = break (== '|') start
    winners = map (read @Int) (words _winners)
    pulls = map (read @Int) (words (drop 1 _pulls))
    winningPulls = filter (`elem` winners) pulls

countWinners :: [String] -> Int
countWinners = foldl reducer 0
  where
    reducer acc line =
      acc + if cnt == 0 then 0 else 2 ^ (cnt - 1)
      where
        cnt = _countWinners line

_buildCounts :: [String] -> [Int] -> [Int]
_buildCounts [] acc = acc
_buildCounts (_ : rest) [] = _buildCounts rest [1]
_buildCounts (line : rest) acc = _buildCounts rest newAcc
  where
    countToCopy = _countWinners line
    previous = take countToCopy acc
    newAcc = 1 + sum previous : acc

buildCounts :: [String] -> [Int]
buildCounts content = _buildCounts content []

part1 :: Bool -> IO ()
part1 example = do
  content <- getInput example
  putStrLn ("Part 1: " ++ show (countWinners $ lines content))

part2 :: Bool -> IO ()
part2 example = do
  content <- getInput example
  putStrLn ("Part 2: " ++ show (sum $ buildCounts $ reverse $ lines content))
