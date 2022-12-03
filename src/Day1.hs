module Day1
    ( day1
    ) where

import Data.List.Split ( splitOn )
import Data.List ( sortBy )

day1 :: String -> IO ()
day1 inp = do
    let data2 = parseInput inp
    print $ part1 data2
    print $ part2 data2

part1 :: [Int] -> Int
part1 = maximum

part2 :: [Int] -> Int
part2 = sum . take 3 . sortBy (flip compare)

parseInput :: String -> [Int]
parseInput = map (sum . map read . filter (/= "") . splitOn "\n") . splitOn "\n\n"
