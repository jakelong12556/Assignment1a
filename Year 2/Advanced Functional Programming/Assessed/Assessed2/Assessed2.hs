-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed2 (choose , simulate , cut , shuffle , riffles , permute , genTree) where

import Types
import Data.List

standard52 :: Deck
standard52 = [Card {rank = r, suit = s} | r <- [R2 .. RA], s <- [C .. S]]

code :: PickingMonad m => m Char
code = do
  i <- pick 0 3
  return ("hello" !! i)

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y,p) <- dist xm, x == y]

normalise :: Eq a => Dist a -> Dist a
normalise xm = Dist [(x,prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x,p) <- dist xm, p > 0]  -- "nub" removes duplicates from a list

instance Show Rank where
  show a = (showRank a)

showRank :: Rank -> String
showRank R2 = "2"
showRank R3 = "3"
showRank R4 = "4"
showRank R5 = "5"
showRank R6 = "6"
showRank R7 = "7"
showRank R8 = "8"
showRank R9 = "9"
showRank R10 = "10"
showRank RJ = "J"
showRank RQ = "Q"
showRank RK = "K"
showRank RA = "A"

      
instance Show Suit where
  show a = (showSuit a)
      
showSuit :: Suit -> String
showSuit C = "\9827"
showSuit D = "\9830"
showSuit H = "\9829"
showSuit S = "\9824"

red, black :: String -> String
red   s = "\x1b[31m" ++ s ++ "\x1b[0m"
black s = "\x1b[30m" ++ s ++ "\x1b[0m"
        
instance Show Card where
  show a = case suit a of
    C -> black (show (rank a) ++ show (suit a))
    D -> red (show (rank a) ++ show (suit a))
    H -> red (show (rank a) ++ show (suit a))
    S -> black (show (rank a) ++ show (suit a))

choose :: PickingMonad m => [a] -> m a
choose [] = error ("enter a valid list")
choose xs = do
    getElem <- pick 0 (length xs - 1)
    return (xs !! getElem)

simulate :: Monad m => m Bool -> Integer -> m Integer
simulate bm 0 = return 0 
simulate bm n|n < 0 = error("n must be positive")
simulate bm n = do
              b <- bm
              xs <- simulate bm (n-1)
              case b of 
                True -> return (1 + xs)
                False -> return xs

cut :: PickingMonad m => [a] -> m ([a],[a])
cut [] = return ([],[])
cut xs = do
    getElem <- pick 0 (length xs)
    return (splitAt (getElem) xs)

shuffle :: PickingMonad m => ([a],[a]) -> m [a]
shuffle ([],[]) = return [] 
shuffle ([], ys) = return ys
shuffle (xs, []) = return xs  
shuffle ((x:xs),(y:ys)) = do
    pickRan <- pick 0 (length (x:xs) + length (y:ys) - 2)
    if (pickRan < (length (x:xs) - 1)) 
      then do
           shuffled <- shuffle (xs, (y:ys))
           return (x:shuffled)
      else do
           shuffled <- shuffle ((x:xs), ys)
           return (y:shuffled)          

riffles :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles cf sf n [] = error("input a list")
riffles cf sf 0 xs = return xs
riffles cf sf n xs = do
  c <- cf xs
  s <- sf c
  riffles cf sf (n-1) s

permute :: PickingMonad m => [a] -> m [a]
permute [] = return [] 
permute (x:xs) = do
  y <- permute xs
  ran <- pick 0 (length xs) 
  return (take ran y ++ [x] ++ drop ran y)

genTree :: PickingMonad m => [a] -> m (Bin a)
genTree = undefined