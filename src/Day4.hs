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

part1 :: [(Range, Range)] -> Int
part1 = evaluatePart (applyFlipped fullyContained)

part2 :: [(Range, Range)] -> Int
part2 = evaluatePart (applyFlipped partiallyContained)

applyFlipped :: (t -> t -> a) -> (t, t) -> [a]
applyFlipped f (x,y) = [f x y, f y x] 

evaluatePart :: (Functor t1, Foldable t1, Foldable t2) => (a -> t2 Bool) -> t1 a -> Int
evaluatePart f = sum . fmap (fromEnum . or . f)

fullyContained :: Range -> Range -> Bool
fullyContained (a,b) (c,d) = a >= c && b <= d

partiallyContained :: Range -> Range -> Bool
partiallyContained (a,_) (c,d) = a >= c && a <= d

parseInput :: String -> [(Range, Range)]
parseInput = map extractEntry . filter (/= "") . splitOn "\n"

extractEntry :: String -> (Range, Range)
extractEntry = (\x -> (extractRange (head x), extractRange (head $ tail x))) .  splitOn ","

extractRange :: String -> Range
extractRange = (\x -> (read (head x), read (head $ tail x))) . splitOn "-"
