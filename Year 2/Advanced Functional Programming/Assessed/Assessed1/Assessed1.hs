-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed1 (doubleList, firstDoubled , priceRange , allergyFree , checkSpec , checkSpec' , linearSort , counterexample , fromBin , toBin) where

import Data.List
import Data.Maybe

import Types

doubleList :: [a] -> [a]
doubleList [] = []
doubleList (x:xs) = [x,x] ++ doubleList xs  

firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled [] = Nothing 
firstDoubled (x:[]) = Nothing
firstDoubled (x:xs) 
                | x == head xs = Just x
                | otherwise = case firstDoubled xs of
                                Nothing -> Nothing 
                                Just n -> Just n 


priceRange :: Price -> Price -> [Cupcake] -> [Cupcake]
priceRange _ _ [] = []
priceRange x y (z:zs)  
        | returnInt x == returnInt y = []
        | returnInt x > returnInt y = []
        | returnInt x < returnInt y = priceRangeStor x y [z] zs
priceRange _ _ (_:_) = []

returnInt :: Price -> Int
returnInt (P a) = a

returnPrice :: Cupcake -> Int 
returnPrice (CC (P b) a) = b

priceRangeStor :: Price-> Price -> [Cupcake] -> [Cupcake] -> [Cupcake]  
priceRangeStor x y [] [] = []
priceRangeStor x y c1 [] = c1
priceRangeStor x y c1 (c2:cs) = 
        if (returnPrice c2 <= returnInt y) && (returnPrice c2 >= returnInt x)
        then priceRangeStor x y (c1 ++ [c2]) cs
        else priceRangeStor x y c1 cs                           

allergyFree :: [Ingredient] -> [Cupcake] -> [Cupcake]
allergyFree [] y = []
allergyFree x [] = []
allergyFree x y = allergyFreeStor x [] y

allergyFreeStor :: [Ingredient] -> [Cupcake] -> [Cupcake] -> [Cupcake]
allergyFreeStor x [] [] = []
allergyFreeStor x y [] = y  
allergyFreeStor x y (z:zs) = 
        if isAllergic x (returnIngredient z)
                then allergyFreeStor x y zs
                else allergyFreeStor x (y ++ [z]) zs

isAllergic :: [Ingredient] -> [Ingredient] -> Bool
isAllergic [] y = False
isAllergic x y = if (head x) `elem` y
                   then True
                   else isAllergic (tail x) y 

returnIngredient :: Cupcake -> [Ingredient]
returnIngredient (CC _ a) = a 

sampletin :: Tin
sampletin = [[Nuts], [Dairy,Gluten], [], [Soy]]

checkSpec :: Spec -> Tin -> Bool
checkSpec (And a b) t = checkSpec a t && checkSpec b t 
checkSpec (Or a b) t = checkSpec a t || checkSpec b t 
checkSpec (Not a) t = not(checkSpec a t) 
checkSpec (HasCup k x) t = x `elem` (t!!k)

checkSpec' :: Spec -> Tin -> Maybe Bool
checkSpec' (And a b) t = if (checkSpec' a t == Nothing || checkSpec' b t == Nothing)
                         then Nothing
                         else Just (fromJust(checkSpec' a t) && fromJust(checkSpec' b t))
checkSpec' (Or a b) t = if (checkSpec' a t == Nothing || checkSpec' b t == Nothing)
                        then Nothing
                        else Just (fromJust(checkSpec' a t) || fromJust(checkSpec' b t))
checkSpec' (Not a) t = if (checkSpec' a t == Nothing)
                        then Nothing
                        else Just (not (fromJust(checkSpec' a t)))
checkSpec' (HasCup k x) t = if k < length t
                            then Just (x `elem` (t!!k))
                            else Nothing                          

linearSort :: Ord a => [a] -> [a]
linearSort [] = []
linearSort (a:ax) = linearSortAux ax [a] []

linearSortAux :: Ord a => [a] -> [a] -> [a] -> [a] 
linearSortAux [] [] c = c
linearSortAux [] (b:bx) c = linearSortAux [] bx (c ++ [b])
linearSortAux (a:ax) [] c = linearSortAux ax [a] c            
linearSortAux (a:ax) (b:bx) c = 
                    if a <= b
                    then linearSortAux ax (a:(b:bx)) c 
                    else linearSortAux (a:ax) bx (c ++ [b]) 

counterexample :: [Int]
counterexample = [4,2,8,3,5,1]

fromBin :: Bin -> [Int]
fromBin = undefined

toBin :: [Int] -> Maybe Bin
toBin = undefined
