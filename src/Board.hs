module Board where

import Data.List.Split (splitOn)

data Player = Red | Blue deriving Show
data Cell =  Stack [Player] | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Blue Blue = True
  (==) Red Red = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Stack xs) (Stack ys) = xs == ys
  (==) _ _ = False

validateFEN :: String -> Bool
validateFEN str =
  let isAllowed = (`elem` "rb,/")
      rows = splitOn "/" str
      correctCommas = all ((== 5) . length . filter (== ',')) rows
  in not (not (all isAllowed str) || (length rows /= 6) || last str == '/' || not correctCommas)

buildBoard :: String -> Board
buildBoard fen =
  let rows = splitOn "/" fen
      buildRow row = map buildCell (splitOn "," row)
      buildCell cell = case cell of
        "" -> Empty
        _  -> Stack $ map color cell
      color c = case c of
        'r' -> Red
        'b' -> Blue
  in map buildRow rows

path :: Pos -> Dir -> Int -> [Pos]
path startPos dir steps = map fst $ take (steps + 1) $ iterate moveReflect (startPos, dir)
  where
    moveReflect (pos, dir) = 
      let newPos = move pos dir in
        if outOfBounds newPos
            then (reflect pos dir, opposite pos dir)
          else (newPos, dir)

    move (Pos c r) direction = case direction of
      North      -> Pos c (r + 1)
      NorthEast  -> Pos (succ c) (r + 1)
      East       -> Pos (succ c) r
      SouthEast  -> Pos (succ c) (r - 1)
      South      -> Pos c (r - 1)
      SouthWest  -> Pos (pred c) (r - 1)
      West       -> Pos (pred c) r
      NorthWest  -> Pos (pred c) (r + 1)

    outOfBounds (Pos c r) = c < 'a' || c > 'f' || r < 1 || r > 6

    reflect (Pos c r) direction = case direction of
      North      -> Pos c (r - 1)
      NorthEast  -> case (c, r) of
          ('f', 6)   -> Pos (pred c) (pred r)
          ('f', _) -> Pos (pred c) (succ r)
          (_, 6) -> Pos (succ c) (pred r)
      East       -> Pos (pred c) r
      SouthEast  -> case (c, r) of
          ('f', 1) -> Pos (pred c) (succ r)
          (_, 1) -> Pos (succ c) (succ r)
          ('f', _)   -> Pos (pred c) (pred r)
      South      -> Pos c (r + 1)
      SouthWest  -> case (c, r) of
        ('a', 1) -> Pos (succ c) (succ r)
        ('a', _) -> Pos (succ c) (pred r)
        (_, 1) -> Pos (pred c) (succ r)
      West       -> Pos (succ c) r
      NorthWest  -> case (c, r) of
        ('a', 6) -> Pos (succ c) (pred r)
        ('a', _) -> Pos (succ c) (succ r)
        (_, 6) -> Pos (pred c) (pred r)

    opposite (Pos c r) direction = case direction of
      North      -> South
      NorthEast  -> case (c, r) of
          ('f', 6)   -> SouthWest
          ('f', _) -> NorthWest
          (_, 6) -> SouthEast
      East       -> West
      SouthEast  -> case (c, r) of
          ('f', 1) -> NorthWest
          (_, 1) -> NorthEast
          ('f', _)   -> SouthWest
      South      -> North
      SouthWest  -> case (c, r) of
        ('a', 1) -> NorthEast
        ('a', _) -> SouthEast
        (_, 1) -> NorthWest
      West       -> East
      NorthWest  -> case (c, r) of
        ('a', 6) -> SouthEast
        ('a', _) -> NorthEast
        (_, 6) -> SouthWest
