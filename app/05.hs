{-# OPTIONS_GHC -Wno-x-partial #-}
module Main (main) where

import System.IO
import Data.List.Split
import Data.Int
import Data.Bifunctor
import Data.Containers.ListUtils (nubOrd)
import Data.List
import Debug.Trace
import Data.Ord

type Long = Int64
type Ingredient = Long
type LowerBound = Long
type UpperBound = Long
type Range = (LowerBound, UpperBound)

parse :: String -> ([Range], [Ingredient])
parse content =
    (
        map (bimap read read
            . parsePair
            . splitOn "-") ranges,
        map read ingredients
    )
    where l = lines content
          (ranges, _:ingredients) = span (/= "") l
          parsePair xs = (head xs, last xs)

inRange :: Ord a => a -> (a, a) -> Bool
inRange i (lowerBound, upperBound) = i >= lowerBound && i <= upperBound

partOne :: String -> Int
partOne input = length . filter (\i -> any (inRange i) ranges) $ ingredients
    where (ranges, ingredients) = parse input

partTwo :: String -> Long
partTwo input = sum $ 
    map (\(lower, upper) -> (upper - lower) + 1) 
        $ foldl ranging [] ranges
    where (ranges, _) = parse input

-- not sure how to avoid the range merging here
-- feels like it should be possible in the list comprehension alone
ranging :: [Range] -> Range -> [Range]
ranging prec r1@(lower, upper) = mergeRanges [ f x | x <- r1 : prec]
    where f r2@(l, u) | lower <  l && upper >= l = (lower, u)
                      | upper >  u && lower <= u = (l, upper)
                      | otherwise                = r2

mergeRanges :: [Range] -> [Range]
mergeRanges = foldr merge [] . sortBy (comparing fst)
  where
    merge r [] = [r]
    merge (lower,upper) acc@((l,u):rs)
        | upper < l = (lower,upper) : acc
        | otherwise = (lower, max upper u) : rs

main :: IO ()
main = do
  let filename = "inputs/5.input"
  content <- openFile filename ReadMode >>= hGetContents
  print $ "Part 1: " <> show (partOne content)
  print $ "Part 2: " <> show (partTwo content)