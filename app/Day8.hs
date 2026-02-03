{-# OPTIONS_GHC -Wno-x-partial #-}

module Day8 (run) where

import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

getInput :: Bool -> IO String
getInput True = readFile "inputs/day-8-example.txt"
getInput False = readFile "inputs/day-8.txt"

run :: Bool -> IO ()
run example = part1 example >> part2 example

type Node = (String, (String, String))

type NodeMap = Map String (String, String)

_parseLine :: String -> Node
_parseLine l = (name, (leftConnection, rightConnection))
  where
    (name, rest) = break (== ' ') l
    (leftConnection, rightConnectionRaw) = break (== ',') $ drop 4 rest
    rightConnection = takeWhile (/= ')') $ drop 2 rightConnectionRaw

parseContent :: String -> (String, Map String (String, String))
parseContent content = (instructions, Map.fromAscList $ sort $ map _parseLine rest)
  where
    splitContent = lines content
    instructions = head splitContent
    rest = drop 2 splitContent

_move :: Char -> NodeMap -> String -> String
_move instruction nodeMap current = case instruction of
  'L' -> fst node
  _ -> snd node
  where
    node = fromJust $ Map.lookup current nodeMap

traverseUntil :: Int -> String -> (String -> Bool) -> String -> NodeMap -> Int
traverseUntil cnt current finish instructions nodeMap
  | finish current = cnt
  | otherwise = traverseUntil (cnt + 1) (_move instruction nodeMap current) finish instructions nodeMap
  where
    instruction = instructions !! rem cnt (length instructions)

part1 :: Bool -> IO ()
part1 example = do
  content <- getInput example
  let (instructions, nodeMap) = parseContent content
  putStrLn ("Part 1: " ++ show (traverseUntil 0 "AAA" (=="ZZZ") instructions nodeMap))

part2 :: Bool -> IO ()
part2 example = do
  content <- getInput example
  let (instructions, nodeMap) = parseContent content
      startPositions = filter ((== 'A') . last) $ Map.keys nodeMap
      allFinished = map (\pos -> traverseUntil 0 pos ((=='Z') . last) instructions nodeMap) startPositions
      result = foldl' lcm 1 allFinished
  putStrLn ("Part 2: " ++ show result)
