{-# OPTIONS_GHC -Wno-x-partial #-}
module Main (main) where
import System.IO
import Data.List.Split
import Data.Int
import Data.Bifunctor (bimap)

type Length = Int
type Long = Int64

type Left = Long
type Right = Long

splitInt :: Long -> Length -> (Left, Right)
splitInt n l = (div n x, mod n x)
             where x = 10 ^ div l 2

splitFirst :: Long -> Length -> Long
splitFirst n l = div n x
             where x = 10 ^ l

digits :: Long -> Int
digits 0 = 0
digits n = 1 + digits (div n 10)

fromNums :: [Long] -> Long
fromNums = fst . foldr addNum (0, 0)
   where addNum num (d, l)  = (num * (10 ^ l) + d, l + digits num)
         digits 0 = 0
         digits n = 1 + digits (div n 10)

isValid' :: Long -> Bool
isValid' x  = notElem x
            . map (fromNums . repl)
            . drop 1
            . take d
            $ iterate (`splitFirst` 1) x
      where d      = digits x
            repl a = replicate (d `div` digits a) a

isValid :: Long -> Bool
isValid x | odd d = True
          | otherwise = l /= r
       where d = digits x
             (l, r) = splitInt x d

sumValidity :: (Long -> Bool) -> String -> Long
sumValidity f = sum
              . concatMap rangeInvalids
              . splitInput
            where splitInput   = map ( bimap r r . parsePair . splitOn "-" ) . splitOn ","
                  parsePair xs = (head xs, last xs)
                  rangeInvalids (a, b) = filter (not . f) [a .. b]
                  r = read

main :: IO ()
main = do
  let filename = "inputs/2.input"
  content <- openFile filename ReadMode >>= hGetContents
  print $ "Part 1: " <> show (sumValidity isValid content)
  print $ "Part 2: " <> show (sumValidity isValid' content)
