module Util where

import Data.Int

type Long = Int64

parsePair :: [a] -> (a, a)
parsePair (a:as) = (a, last as)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

dropEveryOther :: [a] -> [a]
dropEveryOther (x:_:xs) = x : dropEveryOther xs
dropEveryOther [x]      = [x]
dropEveryOther []       = []