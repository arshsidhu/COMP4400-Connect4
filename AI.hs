module AI where

import Data.Maybe
import Debug.Trace(trace)

import Board

-- ## MINMAX STRATEGY ##

maxDepth :: Int
maxDepth = 2

winScore :: Int
winScore = 10000

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
        heuristic   = (evaluateScore board tile) - (evaluateScore board (flipTile tile))
        score       = quickScoreBoard tile board lastMove
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
        heuristic   = -(evaluateScore board tile) + (evaluateScore board (flipTile tile)) 
        score       = quickScoreBoard tile board lastMove
        moves       = validMoves board
        
auxMin :: Int -> Int -> Tile -> Board -> [Move] -> Int
auxMin maxScore _ _ _ [] = maxScore
auxMin maxScore depth tile board (move:moves) 
  | maxScore >= winScore + depth -3
  = maxScore
  | otherwise
  = auxMin (max maxScore (evaluateBoardMax (depth-1) move (flipTile tile) $ put board (flipTile tile) move)) depth tile board moves

-- XX
