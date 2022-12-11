module Day10
    ( day10
    ) where

import Data.List.Split ( splitOn )
import Data.List ( sortBy )

day10 :: String -> IO ()
day10 inp = do
    let c = parseInput inp
    print c
    print $ part1 c
    print $ part2 c

data Instruction = Noop | AddX Int deriving (Show)

part1 :: [Instruction] -> Int
part1 xs = 0

part2 :: [Instruction] -> Int
part2 xs = 0 

parseInstruction s
    | take 4 s == "noop" = Noop
    | otherwise = AddX (read (dropWhile (/= ' ') s))

parseInput :: String -> [Instruction]
parseInput = map parseInstruction . filter (/=""). splitOn "\n"
