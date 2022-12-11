module Day9
    ( day9
    ) where

import Data.List.Split ( splitOn )
import Data.List ( nub )

day9 :: String -> IO ()
day9 inp = do
    let c = parseInput inp
    print $ part1 c
    print $ part2 c

part1 :: (Foldable t1, Num t2, Enum t2) => t1 (Move t2) -> Int
part1 xs = length $ nub cs
    where (_, cs) = foldl moveRope ([(0,0),(0,0)], []) xs

part2 :: (Foldable t1, Num t2, Enum t2) => t1 (Move t2) -> Int
part2 xs = length $ nub cs
    where (_, cs) = foldl moveRope ([(0,0) | _ <- [0..9]], []) xs

type Coord = (Int, Int)
type Rope = [Coord]
data Move a = MoveLeft a | MoveRight a | MoveUp a | MoveDown a deriving (Show)


moveRope :: (Num t, Enum t) => (Rope, [Coord]) -> Move t -> (Rope, [Coord])
moveRope r (MoveLeft v) = multiMove r (MoveLeft 1) v
moveRope r (MoveRight v) = multiMove r (MoveRight 1) v
moveRope r (MoveUp v) = multiMove r (MoveUp 1) v
moveRope r (MoveDown v) = multiMove r (MoveDown 1) v

multiMove :: (Num t, Enum t) => (Rope, [Coord]) -> Move Int -> t -> (Rope, [Coord])
multiMove r m v = foldl (\acc x -> x acc) r [moveRopeSingle m | _ <- [1..v]]

moveRopeSingle :: Move Int -> (Rope, [Coord]) -> (Rope, [Coord])
moveRopeSingle m (h:r,bs) =
    let
        h' = applyMove h m
        r' = foldl (\acc x -> moveTail (head acc) x : acc) [h'] r
    in (reverse r', head r' : bs)
moveRopeSingle _ _ = error "ur rope bad"

applyMove :: Coord -> Move Int -> Coord
applyMove (x,y) m = case m of
    MoveLeft v ->   (x - v, y)
    MoveRight v ->  (x + v, y)
    MoveUp v ->     (x, y + v)
    MoveDown v ->   (x, y - v)

moveTail :: Coord -> Coord -> Coord
moveTail (x,y) (x',y')
    | abs (x - x') > 1 = (moveDir x x', moveDir y y')
    | abs (y - y') > 1 = (moveDir x x', moveDir y y')
    | otherwise = (x', y')

moveDir :: Num a => a -> a -> a
moveDir a b = b + signum (a - b)

parseInput :: String -> [Move Int]
parseInput = map (uncurry parseMove . break (==' ')) . filter (/= "" ) . splitOn "\n"

parseMove :: String -> String -> Move Int
parseMove d v
    | d == "L" = MoveLeft v'
    | d == "R" = MoveRight v'
    | d == "U" = MoveUp v'
    | otherwise = MoveDown v'
    where v' = read v


