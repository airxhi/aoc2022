module Day6
    ( day6
    ) where

import Data.Set ( fromList, size )

day6 :: String -> IO ()
day6 inp = do
    let c = parseInput inp
    print $ part1 c
    print $ part2 c

part1 :: (Ord a, Num t) => [a] -> ([a], t)
part1 c = solve 4 c [] 0

part2 :: (Ord a, Num t) => [a] -> ([a], t)
part2 c = solve 14 c [] 0 

solve :: (Ord a, Num t) => Int -> [a] -> [a] -> t -> ([a], t)
solve n (x:xs) acc i
    | (==n) . size $ fromList acc = (acc, i)
    | otherwise = solve n xs (safeTail acc n ++ [x]) (i+1)
solve _ [] acc i = (acc, i)

safeTail :: [a] -> Int -> [a]
safeTail xs n
    | length xs == n = tail xs
    | otherwise = xs

parseInput :: String -> String
parseInput = filter (/= '\n')
