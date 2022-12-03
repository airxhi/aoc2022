module Day3
    ( day3
    ) where

import Data.List.Split ( splitOn, chunksOf )
import Data.List ( intersect, nub )
import Data.Char ( ord )

day3 :: String -> IO ()
day3 inp = do
    let content = parseInput inp
    print $ part1 content
    print $ part2 content

part1 :: [String] -> Int
part1 = sum . map (getCharValue . findDuplicate . splitInTwo)

part2 :: [String] -> Int
part2 = sum . map (getCharValue . nub . chainIntersect) . chunksOf 3

chainIntersect :: Eq a => [[a]] -> [a]
chainIntersect x = foldr intersect (head x) x

findDuplicate :: Ord a => ([a], [a]) -> [a]
findDuplicate (a,b) = nub a `intersect` nub b

normalizeCharVal :: (Ord a, Num a) => a -> a
normalizeCharVal x
    | x >= 0x61 = x - 0x60
    | otherwise = x - 0x40 + 26

splitInTwo :: [a] -> ([a], [a])
splitInTwo xs =
    let m = length xs `div` 2 in
        splitAt m xs

getCharValue :: [Char] -> Int
getCharValue = normalizeCharVal . ord . head

parseInput :: String -> [String]
parseInput = filter (/= "") . splitOn "\n"