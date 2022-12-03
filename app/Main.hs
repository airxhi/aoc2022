module Main (main) where

import Day1
import Day2


run :: (String -> IO()) -> String -> IO ()
run f fileName =
    do
        contents <- readFile fileName
        f contents
        

main :: IO ()
main = 
    do
        -- run day1 "../2022/day1.txt"
        run day2 "../2022/day2.txt"
