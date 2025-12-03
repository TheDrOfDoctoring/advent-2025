{-# OPTIONS_GHC -Wno-x-partial #-}
module Main (main) where

import System.IO
import Data.Int
import Data.List
import Data.Maybe
import Data.Function
import Data.Ord
import Data.Char (digitToInt)

type Long = Int64
type Size = Int

fromNums :: [Long] -> Long
fromNums = fst . foldr addNum (0, 0)
   where addNum num (d, l)  = (num * (10 ^ l) + d, l + digits num)
         digits 0 = 0
         digits n = 1 + digits (div n 10)

-- very slow compared to the version used in partTwo, this can be replaced with a version of part2 that uses 2 as the Size value
partOne s = sum $ map (maximum . highestConsectutive) l
    where l = map (map (fromIntegral . digitToInt)) s
          highestConsectutive [] = []
          highestConsectutive (a:as) = f (a:as) ++ highestConsectutive as
            where f (x:xs) = [fromNums [x, y] | y <- xs ]
partTwo s = sum $ map (fromNums . largestIn 12) l
    where l = map (map (fromIntegral . digitToInt)) s

largestIn' :: Size -> [Long] -> [Long] -> [Long]
largestIn' s b [] = reverse b
largestIn' s b xs = if length b == s
    then reverse b
    else largestIn' s (digit : b) (drop ( head indices +1) xs)

    where remaining = s - length b
          rest = take (length xs - remaining + 1) xs
          (digit, indices) = (maximum rest, elemIndices (maximum rest) rest)

largestIn :: Size -> [Long] -> [Long]
largestIn s = largestIn' s []

main :: IO ()
main = do
  let filename = "inputs/3.input"
  content <- openFile filename ReadMode >>= hGetContents
  let l = lines content
  print $ "Part 1: " <> show (partOne l)
  print $ "Part 2: " <> show (partTwo l)

