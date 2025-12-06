{-# OPTIONS_GHC -Wno-x-partial #-}
module Main (main) where

import System.IO
import Data.List.Split
import Data.Int
import Data.Bifunctor
import Data.List
import Data.Ord
import Data.Ranges
import Util
type Ingredient = Long

parse :: String -> ([Range], [Ingredient])
parse content =
    (
        map parseRange ranges,
        map read ingredients
    )
    where l = lines content
          (ranges, _:ingredients) = span (/= "") l

partOne :: String -> Int
partOne input = length . filter (\i -> any (inRange i) ranges) $ ingredients
    where (ranges, ingredients) = parse input

partTwo :: String -> Long
partTwo input = sum $
    map (\(Range(lower, upper)) -> (upper - lower) + 1)
        $ mergeRanges ranges
    where (ranges, _) = parse input

main :: IO ()
main = do
  let filename = "inputs/5.input"
  content <- openFile filename ReadMode >>= hGetContents
  print $ "Part 1: " <> show (partOne content)
  print $ "Part 2: " <> show (partTwo content)