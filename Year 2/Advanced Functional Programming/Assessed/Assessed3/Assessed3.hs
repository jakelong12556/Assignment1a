-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed3 where

import Data.List
import Data.Tree

import Types
import DomViz  -- comment out as a last resort if you are unable to install diagrams

-- given a cell c and a player p, compute the adjacent cell c'
-- that is also occupied if p plays a domino at c
adjCell :: Cell -> Player -> Cell
adjCell (x,y) H = (x+1,y)
adjCell (x,y) V = (x,y+1)

-- compute the opponent of a player
opp :: Player -> Player
opp H = V
opp V = H

-- determine whether a move is valid in a given board
valid :: Board -> Cell -> Bool
valid b c = c `elem` free b && adjCell c (turn b) `elem` free b

-- create an empty board from an arbitrary list of cells
empty :: [Cell] -> Board
empty cs = Board { turn = H, free = cs, hist = [] }

-- create a rectangular board of arbitrary dimensions
board :: Int -> Int -> Board
board maxx maxy = empty [(x,y) | x <- [1..maxx], y <- [1..maxy]]

-- create a crosshatch-shaped square board of arbitrary dimension
hatch :: Int -> Board
hatch n = empty [(x,y) | x <- [1..2*n+1], y <- [1..2*n+1], odd y || x == 1 || x == (2*n+1) || odd x]

-- some example Domineering games
board4x4_3 = Board { turn = H,
                     free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)],
                     hist = [(1,3),(2,1)] }

alphaDom_vs_LeeSedom =
  Board { turn = V,
          free = [(-4,1),(-4,3),(-2,0),(-2,4),(2,1),(2,4),(3,-4),(3,4),(4,-2),(4,0)],
          hist = [(0,4),(4,1),(0,-4),(-4,-3),(-1,-2),(2,-1),(-2,-4),(-4,-1),(-1,2),(4,3),(1,2),(-2,2),(-4,-4),(-2,-2),(2,-2),(4,-4),(-3,1),(2,-4),(-4,4),(-1,3),(-4,2),(-3,-2),(3,-1),(1,-3),(-2,-3),(3,1),(1,3)] }

alphaDom_vs_RanDom =
  Board { turn = V,
          free = [(-4,-3),(-4,0),(-2,-4),(-2,-2),(-1,-4),(-1,-2),(-1,2),(-1,4),(0,-4),(0,-2),(0,2),(0,4),(1,-4),(1,-2),(1,2),(1,4),(2,-4),(2,-2),(2,4),(3,-4),(4,0),(4,3)],
          hist = [(-3,4),(2,-1),(-3,2),(4,-2),(-4,-4),(-4,3),(3,4),(2,1),(-3,1),(3,1),(-4,-1),(-2,-1),(-2,3),(-4,1),(1,3),(4,-4),(-4,-2),(4,1),(1,-3),(3,-2),(-2,-3)] }

-- start of Question 1


legalMoves :: Player -> Board -> [Cell]
legalMoves H b = filter (valid Board {turn = H, free = free b, hist = hist b})(free b)
legalMoves V b =  filter (valid Board {turn = V, free = free b, hist = hist b})(free b)


moveLegal :: Board -> Cell -> Board
moveLegal b c = if (valid b c)
                then Board {turn = opp (turn b), free = filter (`notElem` [c, adjCell c (turn b)])(free b), hist = [c] ++ hist b}
                else undefined

replay :: Board -> [Board]
replay b = if (hist b /= []) then replay (Board {turn = opp (turn b), free = sort ([head (hist b), adjCell (head (hist b)) (opp (turn b))] ++ free b), hist = tail (hist b)}) ++ [b]
                             else [b]

gametree :: Board -> Tree Board
gametree b = Node b [gametree (moveLegal b c) | c <- legalMoves (turn b) b]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

score :: Board -> Score
score b = if (legalMoves H b == []) 
            then Win V 
            else if (legalMoves V b == [])
                 then Win H
                 else case (turn b) of 
                  H -> Heu (length (legalMoves V b) - length (legalMoves H b) + 1)
                  V -> Heu (length (legalMoves V b) - length (legalMoves H b) - 1)
                      

minimax :: (Board -> Score) -> Tree Board -> Tree (Board, Score)
minimax score (Node b [])
  | score b == Win H  =  Node (b,Win H) []
  | score b == Win V  =  Node (b,Win V) []
  | otherwise = Node (b, score b) []
minimax score (Node b ts)
  | turn b == H = Node (b, minimum ps) ts'
  | turn b == V = Node (b, maximum ps) ts'
                  where
                     ts' = map (minimax score) ts
                     ps  = [p | Node (_,p) _ <- ts']

bestmoves :: Int -> (Board -> Score) -> Board -> [Cell]
bestmoves d scorefn b = getOpti bestTree 
               where
                  bestTree = [b' | Node (b',c') _ <- ts, c' == best]
                  tree = prune d (gametree b)
                  Node (_,best) ts = minimax scorefn tree

getOpti :: [Board] -> [Cell]
getOpti [] = [] 
getOpti (x:xs) = [head (hist x)] ++ getOpti xs


chooseSafe :: PickingMonad m => [a] -> m (Maybe a)
chooseSafe [] = return Nothing
chooseSafe xs = do
  i <- pick 0 (length xs - 1)
  return (Just (xs !! i))

randomBestPlay :: PickingMonad m => Int -> (Board -> Score) -> Board -> m (Maybe Cell)
randomBestPlay d sfn = chooseSafe . bestmoves d sfn
randomPlay :: PickingMonad m => Board -> m (Maybe Cell)
randomPlay b = chooseSafe (legalMoves (turn b) b)

-- start of Question 3

runGame :: PickingMonad m => (Board -> m (Maybe Cell)) -> (Board -> m (Maybe Cell)) -> Board -> m Board
runGame playH playV b = 
  if (legalMoves H b /= [] && legalMoves V b /= []) 
    then if (turn b == H)
      then do 
         h <- (playH b)
         case h of
           Nothing -> return b
           Just x -> runGame playH playV (moveLegal b x)
      else do 
         h <- (playV b)
         case h of
           Nothing -> return b
           Just x -> runGame playH playV (moveLegal b x)    
 else if (legalMoves H b == [])
      then if (turn b == H)
           then return b
           else do 
            h <- (playV b)
            case h of
              Nothing -> return b
              Just x -> runGame playH playV (moveLegal b x) 
      else if (turn b == V)
           then return b
           else do 
            h <- (playH b)
            case h of
              Nothing -> return b
              Just x -> runGame playH playV (moveLegal b x)                    

-- start of Question 4

carpets :: [Board]
carpets = undefined




