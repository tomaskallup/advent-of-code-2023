module Day2 where

import Data.Char (isDigit)

getInput :: Bool -> IO String
getInput True = readFile "inputs/day-2-example.txt"
getInput False = readFile "inputs/day-2.txt"

data Color = Red | Blue | Green deriving (Eq, Ord, Enum, Show)

data DiceRoll = DiceRoll
  { color :: Color,
    count :: Int
  }
  deriving (Show)

data Game = Game {rounds :: [[DiceRoll]], gameId :: Int} deriving (Show)

run :: Bool -> IO ()
run example = part1 example >> part2 example

parseGameId :: String -> (Int, String)
parseGameId input = _parseGameId input 0

_parseGameId :: String -> Int -> (Int, String)
_parseGameId [] acc = (acc, [])
_parseGameId str@(cs : rest) acc
  | isDigit cs = _parseGameId rest (acc * 10 + read [cs])
  | take 5 str == "Game " = _parseGameId (drop 5 str) acc
  -- This case should drop the `:`
  | otherwise = (acc, rest)

parseDice :: String -> (DiceRoll, String)
parseDice input = _parseDice input DiceRoll {color = Red, count = 0}

_parseDice :: String -> DiceRoll -> (DiceRoll, String)
_parseDice [] dice = (dice, [])
_parseDice str@(cs : rest) dice
  | isDigit cs = _parseDice rest dice {count = count dice * 10 + read [cs]}
  | cs == ' ' = _parseDice rest dice
  | cs == ',' = (dice, rest)
  | take 3 str == "red" = _parseDice (drop 3 str) dice {color = Red}
  | take 4 str == "blue" = _parseDice (drop 4 str) dice {color = Blue}
  | take 5 str == "green" = _parseDice (drop 5 str) dice {color = Green}
  | otherwise = (dice, str)

parseRounds :: String -> [[DiceRoll]]
parseRounds line = _parseRounds line []

_parseRounds :: String -> [[DiceRoll]] -> [[DiceRoll]]
_parseRounds [] acc = reverse acc
_parseRounds str acc =
  let (rolls, rest) = _parseRound str []
   in _parseRounds rest (rolls : acc)

_parseRound :: String -> [DiceRoll] -> ([DiceRoll], String)
_parseRound [] rolls = (rolls, [])
_parseRound str@(cs : rest) rolls
  | cs == ';' = (rolls, rest)
  | otherwise =
      let (dice, rest2) = parseDice str
       in _parseRound rest2 (dice : rolls)

parseGame :: String -> Game
parseGame line =
  let (gameId, rest) = parseGameId line
   in Game
        { gameId,
          rounds = parseRounds rest
        }

parseGames :: String -> [Game]
parseGames input = Prelude.map parseGame (lines input)

limit :: Color -> Int
limit Red = 12
limit Green = 13
limit Blue = 14

_checkRoll :: DiceRoll -> Bool
_checkRoll roll = count roll <= limit (color roll)

_checkRound :: [DiceRoll] -> Bool
_checkRound [] = True
_checkRound rolls = all _checkRoll rolls

_checkGame :: Game -> Bool
_checkGame game = all _checkRound (rounds game)

part1 :: Bool -> IO ()
part1 example = do
  content <- getInput example
  let games = parseGames content
  let valid = filter _checkGame games
  let validIds = map gameId valid
  putStrLn ("Part 1: " ++ show (sum validIds))

data Min = Min
  { red :: Int,
    blue :: Int,
    green :: Int
  }
  deriving (Show)

defaultMin :: Min
defaultMin = Min {red = 0, blue = 0, green = 0}

compareRoll :: DiceRoll -> Min -> Min
compareRoll (DiceRoll Red count) minRecord = minRecord {red = max (red minRecord) count}
compareRoll (DiceRoll Blue count) minRecord = minRecord {blue = max (blue minRecord) count}
compareRoll (DiceRoll Green count) minRecord = minRecord {green = max (green minRecord) count}

compareRound :: [DiceRoll] -> Min -> Min
compareRound rest minRecord =
  foldl (flip compareRoll) minRecord rest

_getMinForGame :: Game -> Min
_getMinForGame game = foldl (flip compareRound) defaultMin (rounds game)

_getPower :: Min -> Int
_getPower (Min red blue green) = red * blue * green

getMinPower :: Game -> Int
getMinPower game = _getPower (_getMinForGame game)

part2 :: Bool -> IO ()
part2 example = getInput example >>= _part2

_part2 :: String -> IO ()
_part2 content =
  let games = parseGames content
   in let minPowers = map getMinPower games
       in putStrLn ("Part 2: " ++ show (sum minPowers))
