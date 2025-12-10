{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, DeriveGeneric, TypeOperators, TypeFamilies #-} 

module Data.PositionMap where

import qualified Data.Map as M
import Data.Maybe
import Data.Hashable
import Data.MemoTrie
import GHC.Generics
data Position = Position {
    x :: Int,
    y :: Int
} deriving (Eq, Ord, Show)



deriving instance Generic Position
instance HasTrie Position where
  newtype (Position :->: b) = PositionTrie { unPositionTrie :: Reg Position :->: b } 
  trie = trieGeneric PositionTrie
  untrie = untrieGeneric unPositionTrie
  enumerate = enumerateGeneric unPositionTrie

instance Hashable Position

type PositionMap = M.Map Position Char
type Bounds = (Int, Int)

distanceTo :: Position -> Position -> Integer
distanceTo a b = fromIntegral $ (a.x-b.x)^2 + (a.y-b.y)^2

downFrom :: Position -> Position
downFrom pos = Position pos.x (pos.y + 1)

adjacentHorizontal :: Position ->[Position]
adjacentHorizontal pos = [Position (pos.x+1) pos.y, Position (pos.x-1) pos.y]

toMap :: [[Char]] -> [(Position, Char)]
toMap l = concatMap
    (\(e, y) ->
        zipWith
            (\e' x -> (Position x y, e'))
            e [0..]
    )
    $ zip l [0..]

adjacentPositions :: Bounds -> Position -> [Position]
adjacentPositions (rightBound, bottomBound) p = filter (/= p) $
        catMaybes [ createPosition (p.x + x) (p.y + y) | x <- [-1..1], y <- [-1..1] ]
    where createPosition x' y' | x' < 0 = Nothing
                               | y' < 0 = Nothing
                               | y' > bottomBound = Nothing
                               | x' > rightBound  = Nothing
                               | otherwise = Just (Position x' y')

mapBounds :: [(Position, Char)] -> Bounds
mapBounds m = (maximum $ map (x . fst) m, maximum $ map (y. fst) m)