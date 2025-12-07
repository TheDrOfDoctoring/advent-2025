module Main (main) where

import System.IO
import Data.PositionMap
import qualified Data.Map as M
import Data.List
import Data.Maybe
import qualified Data.HashSet as Set
import Data.Int
import Data.MemoTrie

type Long = Int64

type BeamPosition = Position
type Visited = Set.HashSet Position

nextBeamPosition :: PositionMap -> BeamPosition -> [BeamPosition]
nextBeamPosition posMap pos = case M.lookup below posMap of
    Just '^'  -> adjacentHorizontal below
    Just _    -> [below]
    Nothing   -> []
  where below = downFrom pos

startingPosition :: PositionMap -> Position
startingPosition = fst . fromJust . find (\(_, c) -> c == 'S') . M.toList

partOne :: PositionMap -> Int
partOne posMap = Set.size $ splits posMap Set.empty (downFrom $ startingPosition posMap)

partTwo :: PositionMap -> Long
partTwo posMap = splits' posMap 0 (downFrom $ startingPosition posMap)
 
splits :: PositionMap -> Visited -> BeamPosition -> Visited
splits posMap visited pos | pos `Set.member` visited = visited
                          | otherwise = case nextBeamPosition posMap pos of
                            []  -> visited
                            [x] -> splits posMap visited x
                            xs  -> foldl (splits posMap) (Set.insert pos visited) xs

splits' :: PositionMap -> Long -> BeamPosition -> Long
splits' posMap = start
    where start :: Long -> BeamPosition -> Long 
          start = memo2 f
          f timelines pos = case nextBeamPosition posMap pos of
            []  -> timelines
            [x] -> start timelines x
            xs  -> sum [start 1 x | x <- xs]

main :: IO ()
main = do
    let filename = "inputs/7.input"
    content <- openFile filename ReadMode >>= hGetContents
    let m = M.fromList $ toMap $ lines content
    print $ "Part 1: " <> show (partOne m)
    print $ "Part 2: " <> show (partTwo m)