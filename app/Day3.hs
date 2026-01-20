module Day3 (run) where

import Data.Char (isDigit)
import Data.List

getInput :: Bool -> IO String
getInput True = readFile "inputs/day-3-example.txt"
getInput False = readFile "inputs/day-3.txt"

run :: Bool -> IO ()
run example = part1 example >> part2 example

data CellRef
  = NumberCell
      { value :: Int,
        digits :: Int,
        cellId :: CellId,
        valid :: Bool
      }
  | CharCell Char
  deriving (Show, Eq)

type CellId = Int

type Cells = [CellRef]

type CellMap = [[CellId]]

getCell :: CellMap -> (Int, Int) -> Maybe CellId
getCell cellMap (x, y) = cellMap !? y >>= \row -> row !? x

appendToMap :: CellMap -> [CellId] -> CellMap
appendToMap [] toAdd = [toAdd]
appendToMap cellMap toAdd = init cellMap ++ [last cellMap ++ toAdd]

replaceCell :: Cells -> Int -> CellRef -> Cells
replaceCell cells cellId cell = start ++ [cell] ++ rest
  where
    (begining, rest) = splitAt (cellId + 1) cells
    start = case begining of
      [] -> []
      _ -> init begining

_updateCell :: Cells -> CellRef -> CellId -> Cells
_updateCell cells (CharCell _) targetCellId =
  case targetCell of
    NumberCell {valid = True} -> cells
    CharCell _ -> cells
    cell@(NumberCell {}) -> replaceCell cells targetCellId cell {valid = True}
  where
    targetCell = cells !! targetCellId
_updateCell cells cell@(NumberCell {valid, cellId}) targetCellId
  | valid = cells
  | otherwise =
      case targetCell of
        CharCell '.' -> cells
        NumberCell {} -> cells
        CharCell _ -> replaceCell cells cellId (cell {valid = True})
  where
    targetCell = cells !! targetCellId

_updateNeighbours :: (Cells, CellMap) -> CellRef -> (Int, Int) -> (Cells, CellMap)
_updateNeighbours (cells, cellMap) cell (x, y) = maybe (cells, cellMap) f (getCell cellMap (x, y))
  where
    f cellId = (_updateCell cells cell cellId, cellMap)

_checkNeighbours :: (Cells, CellMap) -> CellRef -> (Cells, CellMap)
_checkNeighbours (cells, cellMap) (CharCell '.') = (cells, cellMap)
_checkNeighbours (cells, cellMap) cell@(CharCell _) = foldl reducer (cells, cellMap) toCheck
  where
    x = length (last cellMap) - 1
    y = length cellMap - 1
    toCheck = [(x - 1, y), (x + 1, y - 1), (x, y - 1), (x - 1, y - 1)]
    reducer (cellsR, cellMapR) (tx, ty) = _updateNeighbours (cellsR, cellMapR) cell (tx, ty)
_checkNeighbours (cells, cellMap) cell@(NumberCell {digits}) = foldl reducer (cells, cellMap) toCheck
  where
    maxX = length (last cellMap)
    minX = maxX - digits - 1
    y = length cellMap - 1
    toCheck = (minX, y) : [(x, y - 1) | x <- [minX .. maxX]]
    reducer (cellsR, cellMapR) (tx, ty) = _updateNeighbours (cellsR, cellMapR) cell (tx, ty)

_updateState :: (Cells, CellMap) -> (CellRef, Int) -> (Cells, CellMap)
_updateState (cells, cellMap) (cell, len) = _checkNeighbours (updatedCells, updatedMap) cell
  where
    updatedCells = cells ++ [cell]
    updatedMap = appendToMap cellMap (replicate len (length cells))

parseRow :: String -> (Cells, CellMap) -> (Cells, CellMap)
parseRow [] (cells, cellMap) = (cells, cellMap ++ [[]])
parseRow str (cells, cellMap) = maybe (cells, cellMap) f (parseCell str (length cells))
  where
    f (cell, rest) = parseRow rest (_updateState (cells, cellMap) (cell, length str - length rest))

parseCell :: String -> CellId -> Maybe (CellRef, String)
parseCell [] _ = Nothing
parseCell str@(ch : rest) cellId
  | isDigit ch = let (value, digits, unprocessed) = parseNum str (0, 0) in Just (NumberCell {value, digits = digits, valid = False, cellId}, unprocessed)
  | otherwise = Just (CharCell ch, rest)

parseNum :: String -> (Int, Int) -> (Int, Int, [Char])
parseNum [] (num, digits) = (num, digits, [])
parseNum str@(ch : rest) (num, digits)
  | isDigit ch = parseNum rest (num * 10 + read [ch], digits + 1)
  | otherwise = (num, digits, str)

part1 :: Bool -> IO ()
part1 example = do
  content <- getInput example
  let (cells, _cellMap) = foldl (flip parseRow) ([], []) (lines content) in putStrLn ("Part 1: " ++ show (getValidSum cells))

getValidSum :: Cells -> Int
getValidSum [] = 0
getValidSum cells = sum $ cells >>= _getValidValue

_getValidValue :: CellRef -> [Int]
_getValidValue NumberCell {valid = True, value} = [value]
_getValidValue _ = []

part2 :: Bool -> IO ()
part2 example = do
  content <- getInput example
  putStrLn ("Part 2: " ++ show content)
