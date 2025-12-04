{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}

module Util where

import qualified Data.Map as M
import Data.Maybe

data Position = Position {
    x :: Int,
    y :: Int
} deriving (Eq, Ord, Show)

type PositionMap = M.Map Position Char
type Bounds = (Int, Int)

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

bounds :: [(Position, Char)] -> Bounds
bounds m = (maximum $ map (x . fst) m, maximum $ map (y. fst) m)