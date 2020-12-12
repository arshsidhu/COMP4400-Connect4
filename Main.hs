import Graphics.Gloss (play, white, red, green, Display(InWindow))
import Graphics.Gloss.Data.Picture
    (Picture, rectangleSolid, thickCircle, translate, pictures, color, rotate,
     blank)
import Graphics.Gloss.Interface.Pure.Game
    (Event (EventKey), Key (SpecialKey), KeyState (Up), SpecialKey (KeyF1, KeyF2, KeyF3, KeyF4, KeyF4, KeyF5, KeyF6, KeyF7))

import Board

-- GRAPHICS AND USER INPUT

main :: IO ()
main = play
    (InWindow "Connect4" (700, 600) (100, 100))
    white -- background color
    20
    emptyBoard -- initial world
    renderBoard -- function that makes the world a picture
    handleInput -- handle input events
    step -- step 

renderBoard :: Board -> Picture
renderBoard b =
    pictures $ [  --horizontal lines
                  translate 0 200 (rectangleSolid 700 3),
                  translate 0 100 (rectangleSolid 700 3),
                  translate 0 0 (rectangleSolid 700 3),
                  translate 0 (-100) (rectangleSolid 700 3),
                  translate 0 (-200) (rectangleSolid 700 3),
                  --vertical lines
                  translate 250 0 (rectangleSolid 3 600),
                  translate 150 0 (rectangleSolid 3 600),
                  translate 50 0 (rectangleSolid 3 600),
                  translate (-50) 0 (rectangleSolid 3 600),
                  translate (-150) 0 (rectangleSolid 3 600),
                  translate (-250) 0 (rectangleSolid 3 600)
               ] ++ -- X's and 0's
               [ translate (x*100) (y*100) $ 
                renderMarker (b??(relate y, relate x))
               | x <- [-3, -2, -1, 0, 1, 2, 3]
               , y <- [-2.5, -1.5, -0.5, 0.5, 1.5, 2.5] ]

renderMarker :: Tile -> Picture
renderMarker X = color red $ rotate 45 $
                     pictures [rectangleSolid 90 10, rectangleSolid 10 90]
renderMarker O = color green $ thickCircle 35 10
renderMarker _ = blank

relate :: Float -> Int
relate x = case x of
  (-3) -> 1
  (-2) -> 2
  (-1) -> 3
  0 -> 4
  1 -> 5
  2 -> 6
  3 -> 7
  (-2.5) -> 6
  (-1.5) -> 5
  (-0.5) -> 4
  (0.5) -> 3
  (1.5) -> 2
  (2.5) -> 1
  _ -> error "Bad number"

currentPlayer :: Board -> Tile
currentPlayer b | even $ length $ emptySpaces b = X
                | otherwise                   = O

handleInput :: Event -> Board -> Board
handleInput (EventKey (SpecialKey KeyF1) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 1), 1)

handleInput (EventKey (SpecialKey KeyF2) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 2), 2)

handleInput (EventKey (SpecialKey KeyF3) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 3), 3)

handleInput (EventKey (SpecialKey KeyF4) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 4), 4)

handleInput (EventKey (SpecialKey KeyF5) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 5), 5)

handleInput (EventKey (SpecialKey KeyF6) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 6), 6)

handleInput (EventKey (SpecialKey KeyF7) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 7), 7)

handleInput _ b = b

step :: Float -> Board -> Board
step _ b =
    if currentPlayer b == O && (not . null . emptySpaces $ b) && ((tileWins b X) == False) && (tileWins b O == False)
    then put b O (computeMove O b) 
    else b