{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import System.IO
import Data.List.Split
import Data.Char
import Data.Either
import Data.List
import Util

newtype Operation = Operation { operation :: (Integer, Integer -> Integer -> Integer)}

-- I overengineered part one a bit, all of the either stuff is unnecessary.
-- Part 2 is closer to what a simple solution would look like

parseOne :: String -> [([Integer], Operation)]
parseOne input = parseEithers linesMapped
    where lineData    = lines input
          stripped    = map (filter (/="") .endBy " ") lineData
          linesMapped = map (eithers . map parseElement) stripped

-- took far too long to discover Data.List#transpose 

parseTwo :: String -> [([Integer], Operation)]
parseTwo input     = zip (map (map (read . init)) stripped) $ map (\(x:xs) -> parseOperator $ last x) stripped
    where stripped = splitWhen (all (' ' ==)) $ transpose $ lines input

parseEithers :: [Either [Integer] [Operation]] -> [([Integer], Operation)]
parseEithers input       = zip numbers operations
        where numbers    = transpose $ lefts input
              operations = concat    $ rights input

eithers :: [Either a b] -> Either [a] [b]
eithers l = case partitionEithers l of
                  ([], rs) -> Right rs
                  (ls, _)  -> Left ls

eithers' :: [Either a b] -> ([a], b)
eithers' l' = (l, r)
        where (l, r:_) = partitionEithers l'

parseElement :: String -> Either Integer Operation
parseElement input@(a:xs) | isNumber a = Left  $ read input
                          | otherwise  = Right $ parseOperator a

parseOperator :: Char -> Operation
parseOperator y = case y of
    '+' -> Operation (0, (+))
    '*' -> Operation (1, (*))

partOne :: String -> Integer
partOne input = solution d
       where d = parseOne input

partTwo :: String -> Integer
partTwo input = solution d
       where d = parseTwo input

solution :: Foldable t => [(t Integer, Operation)] -> Integer
solution input = sum $ map (\(nums, Operation(identity, func)) -> foldl func identity nums) input

main :: IO ()
main = do
  let filename = "inputs/6.input"
  content <- openFile filename ReadMode >>= hGetContents
  print $ "Part 1: " <> show (partOne content)
  print $ "Part 2: " <> show (partTwo content)