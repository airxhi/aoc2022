module Day8
    ( day8
    ) where

import Data.List.Split ( splitOn )
import Data.List (transpose)

day8 :: String -> IO ()
day8 inp = do
    let c = parseInput inp
    print $ part1 c
    print $ part2 c

part1 :: [[Int]] -> Int
part1 = sum . map fromEnum . findAllMarked . evalRotation . transpose . evalRotation . map (map (\x -> (x,False)))

part2 :: [[Int]] -> Int
part2 c = maximum (evalNeighbours c)

evalNeighbours :: [[Int]] -> [Int]
evalNeighbours xs = [evalNode xs x y | x <- [0 .. maxX], y <- [0 .. maxY]]
    where
        maxX = length (head xs) - 1
        maxY = (length . head $ transpose xs) - 1

evalNode :: [[Int]] -> Int -> Int -> Int
evalNode grid x y = product $ map (lookAtRow2 v) allDirections
    where
        allDirections = getDirections grid x y
        v = (grid !! y) !! x

getDirections :: [[Int]] -> Int -> Int -> [[Int]]
getDirections grid x y = [left, right, up, down]
    where
        left = reverse $ getLeft grid x y
        right = getRight grid x y
        up = reverse $ getUp grid x y
        down = getDown grid x y

getLeft :: [[Int]] -> Int -> Int -> [Int]
getLeft grid x y = ls where (ls, _) = splitAt x (grid !! y)

getRight :: [[Int]] -> Int -> Int -> [Int]
getRight grid x y = rs
    where
        (_, rs') = splitAt x (grid !! y)
        rs = safeExtract rs'

safeExtract :: [Int] -> [Int]
safeExtract (_:xs) = xs
safeExtract [] = []

getUp :: [[Int]] -> Int -> Int -> [Int]
getUp grid x y = getLeft (transpose grid) y x

getDown :: [[Int]] -> Int -> Int -> [Int]
getDown grid x y = getRight (transpose grid) y x

findAllMarked :: [[(a, b)]] -> [b]
findAllMarked = concatMap (map snd)

evalRotation :: [[(Int, Bool)]] -> [[(Int, Bool)]]
evalRotation = map (lookAtRow . reverse . lookAtRow)

lookAtRow2 :: Int -> [Int] -> Int
lookAtRow2 h trees = lookAtRow2' trees h 0

lookAtRow2' :: [Int] -> Int -> Int -> Int
lookAtRow2' (t:trees) maxH counter
    | t < maxH = lookAtRow2' trees maxH (counter + 1)
    | otherwise = counter + 1
lookAtRow2' [] _ counter = counter

lookAtRow :: [(Int, Bool)] -> [(Int, Bool)]
lookAtRow trees = lookAtRow' trees 0 (-1)

lookAtRow' :: [(Int, Bool)] -> Int -> Int -> [(Int, Bool)]
lookAtRow' trees idx maxH
    | idx == length trees = trees
    | otherwise = lookAtRow' (ls ++ [(h, b || h > maxH)] ++ rs) (idx + 1) (max h maxH)
    where (ls, (h,b):rs) = splitAt idx trees

parseInput :: String -> [[Int]]
parseInput = map (map (\x -> read [x])) . filter (/= "\n" ) . splitOn "\n"
