module Main (main) where

import System.IO
import Data.List.Split
import Data.Char
import Data.Maybe
import Data.IntSet (IntSet)
import Data.Set (Set)
import qualified Data.IntSet as Set

import Numeric.LinearProgramming

import Data.List (transpose)
import Linear.Solver (intopt)

type Button = IntSet
type Light = (Int, Bool)

data Machine = Machine { lights :: [Light], buttons :: [Button], joltages :: [Int]}
    deriving (Show, Eq, Ord)

parse :: String -> [Machine]
parse = map lineToMachine . lines

lineToMachine :: String -> Machine
lineToMachine input = Machine lights buttons joltages
    where [_:lights', rest]          = splitOn "]" $ filter (not . isSpace) input
          lights :: [(Int, Bool)]    = zip [0..] $ map (== '#') lights'
          (schematics', [joltages']) = span (\(x:xs) -> x == '(') $ splitOn ")" rest
          buttons :: [IntSet]        = map (\schem -> Set.fromList $ map read (splitOn "," $ drop 1 schem)) schematics'
          joltages :: [Int]          = map read $ splitOn "," $ init $ drop 1 joltages'

composeButton :: Button -> Button -> Button
composeButton a b = Set.union a b `Set.difference` Set.intersection a b

transitions :: [Button] -> [Button] -> [Button]
transitions a b = [composeButton x y | x <- a, y <- b, x /= y ]

solveOne :: Machine -> Int
solveOne machine = result
    where desiredState   = lights machine
          requiredButton = Set.fromList $ mapMaybe (\(i, b) -> if b then Just i else Nothing ) desiredState
          result         = composeUntil requiredButton (buttons machine) (buttons machine) 1

composeUntil :: Button -> [Button] -> [Button] -> Int -> Int
composeUntil desired base buttons amount | desired `elem` buttons = amount
                                         | otherwise              = composeUntil desired base t (amount+1)
            where t = transitions base buttons

partOne :: String -> Int
partOne input = sum $ map solveOne (parse input)

buttonToColumn :: Button -> [Double]
buttonToColumn button = map isPresent [0..11]
               where isPresent n = if Set.member n button then 1 else 0

buttonsToRows :: [Button] -> [[Double]]
buttonsToRows = transpose . map buttonToColumn

machineToConstraints :: Machine -> Constraints
machineToConstraints machine = Dense $ zipWith (:==:) (buttonsToRows btons) jolts
    where (btons, jolts) = (buttons machine, map fromIntegral $ joltages machine)

solveMachine :: Machine -> Double
solveMachine machine = 
    case intopt prob (machineToConstraints machine) [] of
        Feasible (x, _) -> x
        Optimal  (x, _) -> x
        _               -> -10000
    where size = length (buttons machine)
          prob = Minimize $ replicate size 1

partTwo :: String -> Double
partTwo input = sum $ map solveMachine (parse input)

main :: IO ()
main = do
    let filename = "inputs/10.input"
    content <- openFile filename ReadMode >>= hGetContents
    print $ "Part 1: " <> show (partOne content)
    print $ "Part 2: " <> show (partTwo content)