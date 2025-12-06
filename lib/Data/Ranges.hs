module Data.Ranges where

import Data.Int
import Data.Bifunctor
import Data.List.Split (splitOn)
import Data.List (sortBy)
import Data.Ord (comparing)
import Util
type LowerBound = Long
type UpperBound = Long

newtype Range = Range { bounds :: (LowerBound, UpperBound)}
    deriving (Eq, Ord, Show)

inRange :: Long -> Range -> Bool
inRange i (Range(lowerBound, upperBound)) = i >= lowerBound && i <= upperBound

parseRange :: String -> Range
parseRange = Range . bimap read read
            . parsePair
            . splitOn "-"

-- | If one of the range subsumes the other, returns the larger range.
-- Otherwise, returns Nothing
subsumes :: Range -> Range -> Maybe Range
subsumes (Range(lower1, upper1)) (Range(lower2, upper2))
    |   lower1 <= lower2  &&
        upper1 >= upper2
                  = Just $ Range (lower1, upper1)
    |   lower2 <= lower1 &&
        upper2 >= upper1
                  = Just $ Range (lower1, upper1)
    |   otherwise = Nothing

mergeRanges :: [Range] -> [Range]
mergeRanges = foldl ranging []
  where
    ranging prec r1@(Range(lower, upper)) = mergeRanges' (r1 : prec)
    mergeRanges' = foldr merge [] . sortBy (comparing (fst . bounds))
    merge r [] = [r]
    merge (Range(lower,upper)) rest@((Range(l,u)):rs)
        | upper < l = Range (lower,upper) : rest
        | otherwise = Range (lower, max upper u) : rs
