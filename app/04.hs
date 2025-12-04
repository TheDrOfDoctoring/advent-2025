{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where
import System.IO
import qualified Data.Map as M
import Util

type TotalRemoved = Int

hasPaper :: PositionMap -> Position -> Bool
hasPaper posMap pos = case M.lookup pos posMap of
        Nothing -> False
        Just a  -> a == '@'

canBeRemoved :: Bounds -> PositionMap -> Position -> Bool
canBeRemoved b posMap pos = length (filter id $ map (hasPaper posMap) (adjacentPositions b pos) ) < 4

partOne :: [(Position, Char)] -> Int
partOne m = length $ filter id $ map (\ (pos, char) ->
        case char of
            '@' -> canBeRemoved b posMap pos
            _   -> False
        ) m
    where posMap = M.fromList m
          b = bounds m

partTwo :: [(Position, Char)] -> TotalRemoved
partTwo m = snd $ removeRolls b (initial, 0)
    where initial = M.fromList m
          b = bounds m

removeRolls :: Bounds -> (PositionMap, TotalRemoved) -> (PositionMap, TotalRemoved)
removeRolls bounds (m, count) = case toRemove of
                                (x:xs) -> removeRolls bounds (newMap, count + length toRemove)
                                []     -> (m, count)
    where remove pos = M.insert pos '.' m
          toRemove :: [Position] = map fst $ filter (\(pos, char) -> (char == '@') && canBeRemoved bounds m pos) (M.toList m)
          newMap = M.mapWithKey (\pos char -> 
                if pos `elem` toRemove 
                || char == '.' then '.' 
                else '@') m
main :: IO ()
main = do
  let filename = "inputs/4.input"
  content <- openFile filename ReadMode >>= hGetContents
  let m = toMap $ lines content
  print $ "Part 1: " <> show (partOne m)
  print $ "Part 2: " <> show (partTwo m)