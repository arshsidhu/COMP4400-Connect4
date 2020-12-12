module Board where

import qualified Data.List 
import qualified Data.Maybe as M

-- FUNCTIONS DEALING WITH BOARD
type Board  = [(Move, Tile)]
type Move   = (Int,Int)
data Tile = EmptyTile | X | O deriving (Eq, Show)
data Dimentions = Dim {dimN :: Int, dimM :: Int, dimK :: Int}

dim :: Dimentions
dim = Dim 6 7 4

emptyBoard :: Board
emptyBoard = [((x,y), EmptyTile) | x <- [1..(dimN dim)], y <- [1..(dimM dim)]]

(??) :: Board -> Move -> Tile
b??ij = M.fromMaybe EmptyTile (lookup ij b) 

put :: Board -> Tile -> Move -> Board
put b t move = case (putMaybe b t move) of 
  Just a -> a
  Nothing -> error $ show move

putMaybe :: Board -> Tile -> Move -> Maybe Board
putMaybe b t xy = case b??xy of
               EmptyTile -> Just $ map (\(ij,tij) -> if ij == xy then (ij,t) else (ij,tij)) b 
               _         -> Nothing

emptySpaces :: Board -> [(Int, Int)]
emptySpaces b = [(x, y) | x <- [1..6], y <- [1..7], b??(x,y) == EmptyTile]

validMoves :: Board -> [Move]
validMoves board  = [(row', col') | (row', col') <- [(rowOfCol board (dimN dim, col), col) | col <- [4,3,5,2,6,1,7] ], row' /= 0 ]

rowOfCol :: Board -> Move -> Int
rowOfCol board (row, col)
  | board ?? (row, col) == EmptyTile = row
  | otherwise = rowOfCol board (row-1, col)

showBoard :: Board -> String
showBoard b = let blist = boardAsList
              in  unlines [Data.List.intercalate "|" row | row <- blist]
              where
                boardAsList = [[show (b??(x,y)) | y <- [1..dimM dim]] | x <- [1..dimN dim]]

flipTile :: Tile -> Tile
flipTile X = O 
flipTile O = X 
flipTile _ = EmptyTile

tileWins :: Board -> Tile -> Bool
tileWins b t = 
   any (\col -> any (\row -> all (\k -> b??(row,col+k)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row-k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] 