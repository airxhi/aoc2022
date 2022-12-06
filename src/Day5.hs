module Day5
    ( day5
    ) where

import Data.List.Split ( splitOn, chunksOf )
import Data.List.Utils ( replace )
import Data.List ( transpose )

day5 :: String -> IO ()
day5 inp = do
    let (s, m) = parseInput inp
    print $ combineTops $ part1 m s
    print $ combineTops $ part2 m s

part1 :: [(Int, Int, Int)] -> StackSet String -> StackSet String
part1 = foldr ((.) . multiMove) id . reverse

part2 :: [(Int, Int, Int)] -> StackSet a -> StackSet a
part2 = foldr ((.) . multiMove') id . reverse

type Stack a = [a]
type StackSet a = [Stack a]

combineTops :: StackSet String -> String
combineTops = concatMap head

push :: Stack a -> a -> Stack a
push s a = a : s

pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x, xs)
pop [] = error "aaaaaaaaaaaaa"

multiMove :: (Int, Int, Int) -> (StackSet a -> StackSet a)
multiMove (a, b, c) = foldr (.) id (replicate a (move (b, c)))

multiMove' :: (Int, Int, Int) -> (StackSet a -> StackSet a)
multiMove' (a, b, c) s = massPush c s' vs
    where (vs, s') = massPop a b s

move :: (Int,Int) -> StackSet a -> StackSet a
move (a,b) s = stackPush b s' v where (v,s') = stackPop a s

massPush :: Int -> StackSet a -> [a] -> StackSet a
massPush b = foldr (flip (stackPush b))

massPop :: Int -> Int -> StackSet a -> ([a], StackSet a)
massPop a b s = foldr applyFunc ([], s) (replicate a (stackPop b))

applyFunc :: ([Stack a] -> (a, StackSet a)) -> ([a], StackSet a) -> ([a], StackSet a)
applyFunc f (vs, prev) = (vs ++ [v], s)
    where (v, s) = f prev


stackPush :: Int -> [Stack a] -> a -> [Stack a]
stackPush i s v = take i s ++ [push (s !! max 0 i) v] ++ drop (i+1) s

stackPop :: Int -> [Stack a] -> (a, [Stack a])
stackPop i s = (v, e)
    where
        (v, stack) = pop (s !! i)
        e = take i s ++ [stack] ++ drop (i+1) s

triplify :: Num c => [c] -> (c, c, c)
triplify [a,b,c] = (a,b-1,c-1)

parseInput :: String -> (StackSet String, [(Int, Int, Int)])
parseInput xs = (columns, moves)
    where
        split = splitOn "\n\n" xs
        header = map (replace "\n" "" . replace "]" "" . replace "[" "" . replace " " "") . chunksOf 4 $ head split
        numColumns = read (last header)
        columns = map (filter (/= "")) . transpose . init $ chunksOf numColumns header
        moves = map (triplify . map read . splitOn " " . replace "move " "" . replace "from " "" . replace "to " "") . filter (/= "") . splitOn "\n" . head $ tail split
        