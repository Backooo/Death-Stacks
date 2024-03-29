module Deathstacks where

import Board
import Data.List ( nub )
import Data.Char ( ord )
import Data.Maybe (isJust)

data Move = Move {start :: Pos, target :: Pos, steps :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ show startR ++ "-" ++ show tr ++ "-" ++ [tarC] ++ show tarR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 

playerWon :: Board -> Maybe Player
playerWon board =
  let topPlayers = [ head players | Stack players <- concat board]
    in case nub topPlayers of
        [player] -> Just player
        _        -> Nothing

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves pos (Stack colors) =
  nub [Move pos endPos n | n <- [1..length colors], dir <- allDirections, let endPos = last $ path pos dir n, endPos /= pos]
  where
    allDirections = [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]
possibleMoves _ Empty = []

isValidMove :: Board -> Move -> Bool
isValidMove board (Move start _ steps) =
  let startCell = getCell board start
      currentPlayer = getPlayer startCell
      tooTallStackExists = any (any (isTooTallStackOfPlayer currentPlayer)) board
  in case startCell of
    Stack stack | isStackTooTall stack -> length stack - steps <= 4
                | otherwise -> not tooTallStackExists
    _ -> False
  where
    getCell b (Pos c r) = b !! (6 - r) !! (ord c - ord 'a')
    getPlayer (Stack (p:_)) = Just p
    isTooTallStackOfPlayer (Just player) (Stack stack@(p:_)) = p == player && length stack > 4
    isTooTallStackOfPlayer _ _ = False
    isStackTooTall stack = length stack > 4

listMoves :: Board -> Player -> [Move]
listMoves board player
  | isJust (playerWon board) = []
  | otherwise = concatMap filterValidMoves allPlayerCells
  where
    allPlayerCells = [(Pos c (7-r), cell) | (r, row) <- zip [1..6] board,
                                           (c, cell) <- zip ['a'..'f'] row,
                                           isPlayerStack player cell]

    filterValidMoves (pos, cell) = filter (isValidMove board) (possibleMoves pos cell)

    isPlayerStack player (Stack (p:_)) = p == player
    isPlayerStack _ _ = False
