{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
module Main (main) where

import System.IO
import Data.PositionMap
import Data.List.Split
import Util
import Data.List
import Data.Ord
import qualified Data.HashSet as Set
import Data.Containers.ListUtils (nubOrd)
import Control.Parallel.Strategies
import Data.Ranges
import qualified Data.IntMap.Strict as Map
import Data.IntMap.Strict(IntMap)

type Positions = Set.HashSet Position
type Edges = Positions

parse :: String -> [Position]
parse = f . map (splitOn ",") . lines
    where f = map g
          g (a : b : _) = Position (read a) (read b)
          g _ = undefined

area :: Position -> Position -> Integer
area a b = (x + 1) * (y + 1)
    where
    x = abs (fromIntegral $ a.x - b.x)
    y = abs (fromIntegral $ a.y - b.y)

positionsBetween :: Position -> Position -> Positions
positionsBetween a b | a.x == b.x = Set.fromList $ [ Position a.x y | y <- [ (min a.y b.y) .. (max a.y b.y) ] ]
                     | a.y == b.y = Set.fromList $ [ Position x a.y | x <- [ (min a.x b.x) .. (max a.x b.x) ] ]
                     | otherwise  = Set.fromList $ [ Position x y   | 
                                                       x <- [ (min a.x b.x) .. (max a.x b.x) ]
                                                   ,   y <- [ (min a.y b.y) .. (max a.y b.y) ]
                                                   ]
isValid' :: IntMap [Range] -> Positions -> Position -> Position -> Bool
isValid' ranges edges a b = all valid ys
  where
    r = createRange a.x b.x
    ys = if a.y > b.y then [b.y .. a.y] else [a.y .. b.y]
    valid y =
      case Map.lookup y ranges of
        Nothing -> False
        Just rs -> any (isSubsumedBy r) rs

connect :: Position -> [Position] -> [(Position, Position, Integer)]
connect pos = sortOn (Down . (\(_, _, a) -> a)) . map (\other -> (pos, other, area pos other))

sortConnected :: [[(Position, Position, Integer)]] -> [(Position, Position, Integer)]
sortConnected = dropEveryOther . sortOn (Down . (\ (_,_,a) -> a)) . concat

partOne :: String -> Integer
partOne input = area a b
    where l = parse input
          (x:_) = sortConnected $ map (`connect` l) l
          (a,b,_) = x

adjacent :: Position -> Positions
adjacent pos = Set.fromList [ Position (pos.x-1) pos.y, Position (pos.x+1) pos.y, Position pos.x (pos.y-1), Position pos.x (pos.y+1) ]


partTwo :: String -> Integer
partTwo input = maximum areas
    where reds@(x:_:_) = parse input
          allTiles = allValidTiles x Set.empty reds
          edges = Set.filter (\p -> Set.size (adjacent p `Set.intersection` allTiles) == 2) allTiles
          areas = let pairs = [ (x', y') | x' <- reds, y' <- reds]
                  in nubOrd $ parMap rdeepseq
                        (\(x', y') ->
                            if isValid' ranges edges x' y'
                                then area x' y'
                                else 0
                        ) pairs
          ranges  = createRanges edges

createRanges :: Edges -> IntMap [Range]
createRanges edges = Map.fromList ranges
        where grouped = groupBy (\a b -> a.y == b.y) $ sortOn (\pos -> pos.y) $ Set.toList edges
              ranges = map (\positions@(x:xs) -> (x.y, mergeRanges (g [] positions))) grouped
              g :: [Range] -> [Position] -> [Range]
              g ranges []       = ranges
              g ranges [x]      = ranges
              g ranges [x, y]   = createRange x.x y.x : ranges
              g ranges (x:y:xs) = g (createRange x.x y.x : ranges) (y:xs)

allValidTiles :: Position -> Positions -> [Position] -> Positions
allValidTiles first set []       = set
allValidTiles first set [x]      = set `Set.union` positionsBetween x first
allValidTiles first set [x,y]    = positionsBetween y first `Set.union` (set `Set.union` positionsBetween x y)
allValidTiles first set (x:y:xs) = allValidTiles first (set `Set.union` positionsBetween x y) (y:xs)

main :: IO ()
main = do
    let filename = "inputs/9.input"
    content <- openFile filename ReadMode >>= hGetContents
    print $ "Part 1: " <> show (partOne content)
    print $ "Part 2: " <> show (partTwo content)