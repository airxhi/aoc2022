{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day7
    ( day7
    ) where

import Data.List.Split ( splitOn )
import Data.List.Utils ( replace )

day7 :: String -> IO ()
day7 inp = do
    let c = goUpToRoot $ parseInput inp
    print $ part1 c
    print $ part2 c

part1 :: (FileSystem, b) -> Int
part1 (fs, _) = sum . filter (< 100000) . allSizes $ fs

part2 :: (FileSystem, b) -> Int
part2 (fs, _) = minimum $ filter (>head allDirs - (70000000-30000000)) allDirs
    where allDirs = allSizes fs

data Crumb = Crumb String [FileSystem] [FileSystem] deriving (Show)
type Zipper = (FileSystem, [Crumb])
data FileSystem = File Int String | Directory String [FileSystem] deriving (Show)

addDirectory :: String -> Zipper -> Zipper
addDirectory n (Directory d items, bs) = (Directory d (items ++ [Directory n []]), bs)

addFile :: Int -> String -> Zipper -> Zipper
addFile n s (Directory d items, bs) = (Directory d (items ++ [File n s]), bs)

goTo :: Zipper -> String -> Zipper
goTo (Directory n items, bs) d = (item, Crumb n ls rs : bs)
    where (ls, item:rs) = break (dirMatches d) items

goUpToRoot :: Zipper -> Zipper
goUpToRoot (fs, []) = (fs, [])
goUpToRoot (fs, bs) = goUpToRoot (goUp (fs, bs))

goUp :: Zipper -> Zipper
goUp (d, Crumb n ls rs:bs) = (Directory n (ls ++ [d] ++ rs), bs)

dirMatches :: String -> FileSystem -> Bool
dirMatches n (Directory d _) = d == n
dirMatches n (File _ s) = s == n

isFile :: FileSystem -> Bool
isFile (File _ _) = True
isFile (Directory _ _) = False

allSizes :: FileSystem -> [Int]
allSizes (Directory _ items) =
    let dirs = filter (not . isFile) items
        files = filter isFile items
        dirSizes = map allSizes dirs
        directDirs = map head dirSizes
    in sum (map (\(File fSize _) -> fSize) files) + sum directDirs : concat dirSizes

parseInput :: String -> Zipper
parseInput = foldr (handleOperation . replace "\n" "") (Directory "/" [], []) . reverse . filter (/= "") . tail . splitOn "\n"

handleOperation :: String -> Zipper -> Zipper
handleOperation s z
    | isLs s = z
    | isCd s = whichCd s z
    | isDir s = addDirectory (drop 4 s) z
    | otherwise =
        let (fsize, fname) = break (==' ') s
        in addFile (read fsize) fname z

isLs :: String -> Bool
isLs = (=="$ ls")
isCd :: String -> Bool
isCd = (=="$ cd") . take 4
isDir :: String -> Bool
isDir = (=="dir") . take 3
whichCd :: String -> Zipper -> Zipper
whichCd s z
    | (=="$ cd ..") (take 7 s) = goUp z
    | otherwise = goTo z (drop 5 s)
