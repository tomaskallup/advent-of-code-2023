module Main (main) where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import System.Console.GetOpt
import System.Environment
import System.Exit

newtype Options = Options
  {optExample :: Bool}
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optExample = False
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['e'] ["example"] (NoArg (\opts -> opts {optExample = True})) "Run using the example input"
  ]

header :: String
header = "Usage: aoc [OPTIONS] day"

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

main :: IO ()
main = do
  argv <- getArgs
  (opts, args) <- compilerOpts argv
  parse args opts

parse :: [String] -> Options -> IO ()
parse [day] opts = _run (read day :: Integer) opts
parse _ _ = usage >> exit

usage :: IO ()
usage = putStrLn header

_run :: (Eq a, Show a, Num a) => a -> Options -> IO ()
_run 1 opts = Day1.run (optExample opts) >> exit
_run 2 opts = Day2.run (optExample opts) >> exit
_run 3 opts = Day3.run (optExample opts) >> exit
_run 4 opts = Day4.run (optExample opts) >> exit
_run 5 opts = Day5.run (optExample opts) >> exit
_run 6 opts = Day6.run (optExample opts) >> exit
_run 7 opts = Day7.run (optExample opts) >> exit
_run 8 opts = Day8.run (optExample opts) >> exit
_run 9 opts = Day9.run (optExample opts) >> exit
_run day _ = putStrLn ("Day " ++ show day ++ " not implemented") >> exit

exit :: IO a
exit = exitSuccess
