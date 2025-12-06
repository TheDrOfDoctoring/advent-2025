module Main (main) where

import System.IO
import Prelude hiding (Right, Left)

data Direction = Left | Right
    deriving (Eq, Ord, Show)

data Rotation = Rotation
    { amount :: Int
    , direction :: Direction
    }
    deriving (Eq, Ord, Show)

type ZeroCount = Int

rotation :: Rotation -> Int
rotation _a@(Rotation amount direction) = case direction of
            Left  -> -amount
            Right -> amount

parseRotations :: [String] -> [Rotation]
parseRotations = map (\ (x : xs) -> Rotation (read xs) (direction x))
    where direction d = case d of
                    'L' -> Main.Left
                    _   -> Main.Right

rotateBy :: Int -> Rotation -> Int
rotateBy x r | y > 99 = rotateBy (y - 100) (Rotation r' (direction r))
             | y < 0  = rotateBy (100 + y) (Rotation r' (direction r))
             | amount == 0 = y
             | otherwise = y

    where r' = rotation r - amount
          amount = min (rotation r) 100
          y = x + amount

rotateBy' :: (Int, ZeroCount) -> Rotation -> (Int, ZeroCount)
rotateBy' (x, zeros) r | y == 100 && r' == 0 = (0, zeros)
                       | y > 99 = rotateBy' (y - 100, zeros +1) (Rotation r' (direction r))
                       | y < 0  = rotateBy' (100 + y, if x == 0 then zeros else zeros +1) (Rotation r' (direction r))
                       | amount == 0 = (y, zeros)
                       | otherwise = (y, zeros)

    where amount = min (rotation r) 100
          r' = rotation r - amount
          y = x + amount

partOne :: String -> Int
partOne = length . filter (==0) . scanl rotateBy 50 . parseRotations . lines

partTwo :: String -> Int
partTwo x = partOne x + (snd . foldl rotateBy' (50, 0) . parseRotations $ lines x)

main :: IO ()

main = do
  let filename = "inputs/1.input"
  content <- openFile filename ReadMode >>= hGetContents
  print $ "Part 1 Password: " <> show (partOne content)
  print $ "Part 2 Password: " <> show (partTwo content)