module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.List.Split (splitOn)

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

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


-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

validateFEN :: String -> Bool
validateFEN str =
  let isAllowed = (`elem` "rb,/")
      rows = splitOn "/" str
      correctCommas = all ((== 5) . length . filter (== ',')) rows
  in not (not (all isAllowed str) || (length rows /= 6) || last str == '/' || not correctCommas)


-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

buildBoard :: String -> Board
buildBoard fen = 
  if not (validateFEN fen)
  then error "Invalid FEN string"
  else let rows = splitOn "/" fen
           buildRow row = map buildCell (splitOn "," row)
           buildCell cell = case cell of
            "" -> Empty
            _  -> Stack $ map color cell
           color c = case c of
            'r' -> Red
            'b' -> Blue
            _   -> error "Invalid cell string"
       in map buildRow rows


-- #############################################################################
-- #################### path :: Pos -> Dir -> Int -> [Pos]  ####################
-- #################### - 4 Functional Points               ####################
-- #################### - 1 Coverage Point                  ####################
-- #############################################################################

path :: Pos -> Dir -> Int -> [Pos]
path _ _ _ = []