{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main (main) where

import System.IO
import Data.List
import Data.List.Split
import Data.Ord
import Data.Maybe
import Control.Applicative
import Util
data Position = Position {
    x :: Int,
    y :: Int,
    z :: Int
} deriving (Eq, Ord, Show)

type Circuit = [Position]

distanceTo :: Position -> Position -> Integer
distanceTo a b = fromIntegral $ (a.x-b.x)^2 + (a.y-b.y)^2 + (a.z-b.z)^2

parse :: String -> [Position]
parse = f . map (splitOn ",") . lines
    where f = map g
          g (a : b : c : _) = Position (read a) (read b) (read c)
          g _ = undefined

sortConnected :: [[(Position, Position, Integer)]] -> [(Position, Position, Integer)]
sortConnected = dropEveryOther . sortOn (\ (_,_,a) -> a) . concat

findListWith :: (Eq a) => a -> [[a]] -> Maybe ([a], a)
findListWith e l = if len > 1 then fmap (, e) m else Nothing
    where m   = find (elem e) l
          len = maybe 0 length m

other :: Eq p => p -> p -> p -> p
other a b x = if a == x then b else a

mergeOne :: Int -> [(Position, Position, Integer)] -> [Circuit] -> [Circuit]
mergeOne _ [] circuits   = circuits
mergeOne orig ((a,b,_):as) circuits | orig == 0 = circuits

                                    | Just (circuit, x) <- findListWith a circuits <|> findListWith b circuits
                                    = mergeFound circuit x

                                    | otherwise = merge' $ [a,b] : ( circuits \\ [[a],[b]])

    where mergeFound circuit x | let y = other a b x
                               , y `elem` circuit
                               = merge' circuits

                               | Just (circuit', _) <- findListWith (other a b x) circuits
                               = merge' $ (circuit' `union` circuit)
                                        : (circuits \\ [circuit', circuit])

                               | otherwise
                               , let y = other a b x
                               = merge' $ (y : circuit) : (circuits \\ [[y], circuit])
          merge' = mergeOne (orig-1) as

mergeTwo' :: Maybe (Position, Position) -> Int -> [(Position, Position, Integer)] -> [Circuit] 
             -> (Maybe (Position, Position), [Circuit])
mergeTwo' p count [] circuits = (Nothing, circuits)
mergeTwo' p count ((a,b,_):as) circuits | [x] <- map length circuits
                                        ,  x == count 
                                        = (p, circuits)

                                        | Just (circuit, x) <- findListWith a circuits <|> findListWith b circuits
                                        = mergeFound merge' circuit x

                                        | otherwise = merge' $ [a,b] : ( circuits \\ [[a],[b]])
    where t = Just (a,b)
          merge' = mergeTwo' t count as
          mergeFound f circuit x | let y = other a b x
            , y `elem` circuit
            = f circuits

            | Just (circuit', _) <- findListWith (other a b x) circuits
            = f $ (circuit' `union` circuit)
                    : (circuits \\ [circuit', circuit])

            | otherwise
            , let y = other a b x
            = f $ (y : circuit) : (circuits \\ [[y], circuit])

connect :: Position -> [Position] -> [(Position, Position, Integer)]
connect pos = drop 1 . sortOn (\(_, _, a) -> a) . map (\other -> (pos, other, distanceTo pos other))

partOne :: String -> Int
partOne input = product $ take 3 $ sortBy (comparing Down) $ map length $ mergeOne x c []
    where l = parse input
          c = sortConnected $ map (`connect` l) l
          x = length l

partTwo :: String -> Int
partTwo input = a.x * b.x
    where l = parse input
          c = sortConnected $ map (`connect` l) l
          (a,b) = fromJust $ fst $ mergeTwo' Nothing (length l) c []

main :: IO ()
main = do
    let filename = "inputs/8.input"
    content <- openFile filename ReadMode >>= hGetContents
    print $ "Part 1: " <> show (partOne content)
    print $ "Part 2: " <> show (partTwo content)