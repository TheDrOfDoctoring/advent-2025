module Main (main) where

import System.IO
import Data.List
import Data.List.Split
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import Data.MemoTrie

type ID = String
type Outputs = [String]
type Necessary = [ID]

type Devices = Map ID Outputs

parse :: String -> Devices
parse = Map.fromList . map lineToDevice . lines

lineToDevice :: String -> (ID, Outputs)
lineToDevice line        = (id, outputs)
    where [id, rest]     = splitOn ":" line
          (_  : outputs) = splitOn " " rest

partOne :: String -> Integer
partOne = findPaths [] "you" "out" . parse

partTwo :: String -> Integer
partTwo = findPaths ["dac", "fft"] "svr" "out" . parse

findPaths :: Necessary -> ID -> ID -> Devices -> Integer
findPaths required a b devices = start [] a b
    where start :: [String] -> ID -> ID -> Integer
          start = memo3 f
          f :: [String] -> ID -> ID -> Integer
          f visited from end 
                      | end `elem` toGo
                            = if not (null required) then
                                    if intersect required visited == required then 1 else 0
                              else 1
                      | null toGo = 0
                      | otherwise = sum $ map (\a -> start v a end) toGo
            where toGo = fromJust $ Map.lookup from devices
                  -- don't make the mistake of adding every visited node
                  -- unless you have an infinite tape, anyway.
                  v = if from `elem` required then from : visited else visited

main :: IO ()
main = do
    let filename = "inputs/11.input"
    content <- openFile filename ReadMode >>= hGetContents
    print $ "Part 1: " <> show (partOne content)
    print $ "Part 2: " <> show (partTwo content)