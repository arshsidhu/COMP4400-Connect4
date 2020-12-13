-- COMP4400 Final Project
-- Arshdeep Sidhu 104567665
-- Ferruccio Sisti 104807246
-- File containing "AI" code and some helper end game functions

module AI where

import Data.Maybe
import Debug.Trace(trace)

import Board

-- MINMAX STRATEGY

maxDepth :: Int
maxDepth = 2

winScore :: Int
winScore = 10000

-- debug variable used in testing
-- to turn on command line logs, switch bool to True

debug :: Bool
debug = False

computeMove :: Tile -> Board -> Move
computeMove tile board 
  = snd $ maximum scoredMoves 
  where
        scoredMoves = zip scores moves
        scores      = [evaluateBoardMax (maxDepth-1) x tile $ put board tile x | x <- moves]
        moves       = validMoves board

evaluateBoardMax :: Int -> Move -> Tile -> Board -> Int
evaluateBoardMax depth lastMove tile board 
  | isJust score
  = if debug == True
      then trace ("win at move: " ++ show (maxDepth - depth)) $ fromJust score + depth
      else fromJust score + depth
  | depth == 0 || moves == []
  = if debug == True
      then trace ("lastMoveMH: "++show(lastMove)++" ,move# "++show(maxDepth-depth)++" ,score "++show(heuristic)++" "++replicate (depth *16) '*'++"\n"++showBoard board) $ heuristic 
      else heuristic
  | otherwise
  = if debug == True
      then trace ("lastMoveM: "++show(lastMove)++" ,move# "++show(maxDepth-depth)++" ,score "++show(auxMax (minBound::Int) depth tile board moves)++" "++replicate (depth *16) '*'++"\n"++showBoard board) $ auxMax (maxBound::Int) depth tile board moves
      else auxMax (maxBound::Int) depth tile board moves
  where
        heuristic   = (evalScore board tile) - (evalScore board (flipTile tile))
        score       = checkScore tile board lastMove
        moves       = validMoves board
        
auxMax :: Int -> Int -> Tile -> Board -> [Move] -> Int
auxMax minScore _ _ _ [] = minScore
auxMax minScore depth tile board (move:moves) 
  | minScore <= -(winScore + depth -3)
  = minScore
  | otherwise
  = auxMax (min minScore (evaluateBoardMin (depth-1) move (flipTile tile) $ put board (flipTile tile) move)) depth tile board moves

evaluateBoardMin :: Int -> Move -> Tile -> Board -> Int
evaluateBoardMin depth lastMove tile board
  | isJust score
  = if debug == True
      then trace ("loose at move: " ++ show (maxDepth - depth)) $ - (fromJust score + depth)
      else - (fromJust score + depth)
  | depth == 0 || moves == []
  = if debug == True
      then trace ("lastMoveOH: "++show(lastMove)++" ,move# "++show(maxDepth-depth)++" ,score "++show(heuristic)++" "++replicate (depth *16) '*'++"\n"++showBoard board) $ heuristic 
      else heuristic
  | otherwise
  = if debug == True
      then trace ("lastMoveO: "++show(lastMove)++" ,move# "++show(maxDepth-depth)++" ,score "++show(auxMin (minBound::Int) depth tile board moves)++" "++replicate (depth *16) '*'++"\n"++showBoard board) $ auxMin (minBound::Int) depth tile board moves
      else auxMin (minBound::Int) depth tile board moves
  where
        heuristic   = -(evalScore board tile) + (evalScore board (flipTile tile)) 
        score       = checkScore tile board lastMove
        moves       = validMoves board
        
auxMin :: Int -> Int -> Tile -> Board -> [Move] -> Int
auxMin maxScore _ _ _ [] = maxScore
auxMin maxScore depth tile board (move:moves) 
  | maxScore >= winScore + depth -3
  = maxScore
  | otherwise
  = auxMin (max maxScore (evaluateBoardMax (depth-1) move (flipTile tile) $ put board (flipTile tile) move)) depth tile board moves

-- HELPER FUNCTIONS -- 

checkWinningTile :: Board -> Tile -> Move -> Bool
checkWinningTile b t m = 
   any (\col -> any (\row -> all (\k -> b??(row,col+k)   == t) [0..dimK']) [fst m]) [(snd m - dimK')..(snd m)] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col)   == t) [0..dimK']) [(fst m - dimK')..(fst m)]) [snd m] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col+k) == t) [0..dimK']) [(fst m - dimK')..(fst m)]) [(snd m - dimK')..(snd m)] ||
   any (\col -> any (\row -> all (\k -> b??(row-k,col+k) == t) [0..dimK']) [(fst m + dimK'), (fst m + dimK')-1..(fst m)]) [(snd m - dimK')..(snd m)]
   where
     dimK' = (dimK dim)-1

checkScore :: Tile -> Board -> Move -> Maybe Int 
checkScore tile board move
  | checkWinningTile board tile move
  = Just winScore
  | otherwise
  = Nothing

data Pos = Pos {getRow::Int, getCol:: Int}
    deriving (Show, Eq, Ord)

getSymbol :: Pos -> Board -> Maybe Tile
getSymbol position board 
--fromMaybe Nothing (lookup position b)
  | (x < 1) || (x > dimN dim) || (y < 1) || (y > dimM dim) = Nothing
  | otherwise = Just $ board??(x,y)
  where 
    x = getRow position
    y = getCol position

--Evaluates end of the n-tile combination (which has an empty tile on the other end) and returns a score key depending on it
evalEnding :: Int -> Tile -> Maybe Tile -> Int
evalEnding n tile end
  | isNothing end || fromJust end == (flipTile tile)
  = rating n
  | otherwise
  = rating (n+10)

--Evaluates end of the n-tile combination (which has an non-empty tile on the other end) and returns a score key depending on it
evalAltEnding :: Int -> Tile -> Maybe Tile -> Int
evalAltEnding n tile end
  | isNothing end || fromJust end == (flipTile tile)
  = 0
  | otherwise
  = rating n

countRow :: Pos -> Tile -> Board -> Int -> Int
countRow position tile board number
  | getSymbol nextPosition board == Nothing
  = score
  | symbol == tile
  = countRow nextPosition tile board (number + 1)
  | otherwise
  = score + countRow nextPosition tile board 0
  where
    (Pos a b) = position
    nextPosition = Pos a (b+1)
    symbol = fromJust (getSymbol position board)
    score =
      if symbol == EmptyTile
        then evalEnding number tile $ getSymbol (Pos a (b-number-1)) board
        else if symbol == tile
          then evalAltEnding (number+1) tile $ getSymbol (Pos a (b-number)) board
          else evalAltEnding number tile $ getSymbol (Pos a (b-number-1)) board

countCol :: Pos -> Tile -> Board -> Int -> Int
countCol position tile board number
  | getSymbol nextPosition board == Nothing
  = score
  | symbol == tile
  = countCol nextPosition tile board (number + 1)
  | otherwise
  = score + countCol nextPosition tile board 0
  where
    (Pos a b) = position
    nextPosition= Pos (a+1) b
    symbol = fromJust (getSymbol position board)
    score =
      if symbol == EmptyTile
        then evalEnding number tile $ getSymbol (Pos (a-number-1) b) board
        else if symbol == tile
          then evalAltEnding (number+1) tile $ getSymbol (Pos (a-number) b) board --change
          else evalAltEnding number tile $ getSymbol (Pos (a-number-1) b) board

countDiagN :: Pos -> Tile -> Board -> Int -> Int
countDiagN position tile board number
  | getSymbol nextPosition board == Nothing
  = score
  | symbol == tile
  = countDiagN nextPosition tile board (number + 1)
  | otherwise
  = score + countDiagN nextPosition tile board 0
  where
    (Pos a b) = position
    nextPosition = Pos (a+1) (b+1)
    symbol = fromJust (getSymbol position board)
    score =
      if symbol == EmptyTile
        then evalEnding number tile $ getSymbol (Pos (a-number-1) (b-number-1)) board
        else if symbol == tile
          then evalAltEnding (number+1) tile $ getSymbol (Pos (a-number) (b-number)) board
          else evalAltEnding number tile $ getSymbol (Pos (a-number-1) (b-number-1)) board
          
countDiagP :: Pos -> Tile -> Board -> Int -> Int
countDiagP position tile board number
  | getSymbol nextPosition board == Nothing
  = score
  | symbol == tile
  = countDiagP nextPosition tile board (number + 1)
  | otherwise
  = score + countDiagP nextPosition tile board 0
  where
    (Pos a b) = position
    nextPosition = Pos (a-1) (b+1)
    symbol = fromJust (getSymbol position board)
    score =
      if symbol == EmptyTile
        then (evalEnding number tile $ getSymbol (Pos (a+number+1) (b-number-1)) board)
        else if symbol == tile
          then (evalAltEnding (number+1) tile $ getSymbol (Pos (a+number) (b-number)) board)
          else (evalAltEnding number tile $ getSymbol (Pos (a+number+1) (b-number-1)) board)

rating :: Int -> Int
rating number
  | number == 2 = 1
  | number == 3 = 10
  | number == 12 = 20
  | number == 13 = 100
  | otherwise = 0

evalScore :: Board -> Tile -> Int
evalScore board symbol
  | debug == True = trace ("^ scores: " ++ show ([scoreRow,scoreCol,scoreDiagN1 , scoreDiagN2 ,scoreDiagP1 , scoreDiagP2])++" -> [Row,Col,N1,N2,P1,P2]") $ scoreRow + scoreCol + scoreDiagN1 + scoreDiagN2 + scoreDiagP1 + scoreDiagP2
  | otherwise = scoreRow + scoreCol + scoreDiagN1 + scoreDiagN2 + scoreDiagP1 + scoreDiagP2
  where 
    scoreRow =  sum $ map (\x -> countRow (Pos x 1)  symbol board 0) [1..6]
    scoreCol = sum $ map (\y -> countCol (Pos 1 y) symbol board 0) [1..7]
    
    scoreDiagN1 =  sum $ map (\y -> countDiagN (Pos 1 y) symbol board 0) [2..4]
    scoreDiagN2 =  sum $ map (\x -> countDiagN (Pos x 1) symbol board 0) [1..3]

    scoreDiagP1 =  sum $ map (\y -> countDiagP (Pos 6 y) symbol board 0) [1..4]
    scoreDiagP2 =  sum $ map (\x -> countDiagP (Pos x 1) symbol board 0) [4..5]

    