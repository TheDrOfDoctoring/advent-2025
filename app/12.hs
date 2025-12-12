module Main (main) where

import System.IO
import Data.List.Split
import Data.Maybe

type Required   = Int
type RequiredID = Int
type Shape      = (Int, [[Bool]])
type Region     = (Int, Int, [(RequiredID, Required)])

parse :: String -> ([Shape], [Region])
parse input       = (shapes, regions)
    where l       = filter (/= "") $ lines input
          shapes  = zipWith (curry parseShape) [0..] (take 6 $ chunksOf 4 l)
          regions = map (parseLine shapes) $ drop 24 l

parseShape :: (Int, [String]) -> Shape
parseShape (i, l) = (i, map (map (== '#')) l)

parseLine :: [Shape] -> String -> Region
parseLine shapes line = (a, b, required)
    where [start, rest] = splitOn ": " line
          [a, b]        = map read $ splitOn "x" start
          reqs          = map read (splitOn " " rest)
          required      = zip [0..] reqs

partOne :: String -> Int
partOne input = length $ filter id $ map (solveRegion shapes) regions
    where (shapes, regions) = parse input

solveRegion :: [Shape] -> Region -> Bool
solveRegion shapes region@(a, b, requirements) = (a * b) >= require
    where require = sum $ map (\(id, amount) -> areaOfShape (fromJust $ lookup id shapes) * amount) requirements

areaOfShape :: [[Bool]] -> Int
areaOfShape = sum . map (length . filter id)

main :: IO ()
main = do
    let filename = "inputs/12.input"
    content <- openFile filename ReadMode >>= hGetContents
    print $ "Part 1: " <> show (partOne content)