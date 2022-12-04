module Day4
    ( day4
    ) where

import Data.List.Split ( splitOn )

day4 :: String -> IO ()
day4 inp = do
    let content = parseInput inp
    print $ part1 content
    print $ part2 content

type Range = (Int, Int)
type Part4Func = Range -> Range -> Bool

part1 :: [(Range, Range)] -> Int
part1 = evaluatePart fullyContained

part2 :: [(Range, Range)] -> Int
part2 = evaluatePart partiallyContained

evaluatePart :: Part4Func -> [(Range, Range)] -> Int
evaluatePart f = sum . fmap (fromEnum . uncurry f)

fullyContained :: Range -> Range -> Bool
fullyContained (a,b) (c,d) = (a >= c && b <= d) || (c >= a && d <= b)

partiallyContained :: Range -> Range -> Bool
partiallyContained (a,b) (c,d) = (a >= c && a <= d) || (c >= a && c <= b)

parseInput :: String -> [(Range, Range)]
parseInput = map extractEntry . filter (/= "") . splitOn "\n"

extractEntry :: String -> (Range, Range)
extractEntry = (\x -> (extractRange (head x), extractRange (head $ tail x))) .  splitOn ","

extractRange :: String -> Range
extractRange = (\x -> (read (head x), read (head $ tail x))) . splitOn "-"
