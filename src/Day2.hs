{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day2
    ( day2
    ) where

import Data.List.Split ( splitOn )

day2 :: String -> IO ()
day2 inp = do
    let data2 = parseInput inp
    print $ part1 data2
    print $ part2 data2

part1 :: [(Shape, Shape)] -> Int
part1 = sum . map part1Game

part2 :: [(Shape, Shape)] -> Int
part2 = sum . map part2Game


part1Game :: (Shape, Shape) -> Int
part1Game (a,b) =
    let 
        gameScore = resultPoints $ evalGame (b,a)
        moveScore = shapePoints b
    in
    gameScore + moveScore


part2Game :: (Shape, Shape) -> Int
part2Game (a,b) =
    let 
        gameScore = resultPoints $ shapeToResult b
        moveScore = ((shapePoints a - 2 + gameScore `div` 3) `mod` 3) + 1
    in
    gameScore + moveScore


data Shape = Rock | Paper | Scissors deriving (Show, Eq, Enum, Bounded)
data Result = Lose | Draw | Win deriving (Show, Eq)

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred c 
    | c == minBound = maxBound
    | otherwise = pred c

  csucc :: a -> a
  csucc c
    | c == maxBound = minBound
    | otherwise = succ c

instance CyclicEnum Shape


evalGame :: (Shape, Shape) -> Result
evalGame (a,b) 
    | a == b = Draw
    | a == cpred b = Lose
    | a == csucc b = Win 

shapePoints :: Num p => Shape -> p
shapePoints s 
    | s == Rock = 1
    | s == Paper = 2
    | s == Scissors = 3

resultPoints :: Num p => Result -> p
resultPoints r
    | r == Lose = 0
    | r == Draw = 3
    | r == Win = 6

shapeToResult :: Shape -> Result
shapeToResult s
    | s == Rock = Lose
    | s == Paper = Draw
    | s == Scissors = Win

parseInput :: String -> [(Shape,Shape)]
parseInput =
    let
        tupleify [a,b] = (a,b)
        matchShape "X" = Rock
        matchShape "Y" = Paper
        matchShape "Z" = Scissors
        matchShape "A" = Rock
        matchShape "B" = Paper
        matchShape "C" = Scissors
        matchShape _ = Rock
    in
    map (tupleify . map matchShape . splitOn " ") . filter (not . null) . splitOn "\n"
