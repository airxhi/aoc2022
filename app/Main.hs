module Main (main) where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7

run :: (String -> IO()) -> String -> IO ()
run f fileName =
    do
        contents <- readFile fileName
        f contents
        

main :: IO ()
main = 
    do
        -- run day1 "../2022/day1.txt"
        -- run day2 "../2022/day2.txt"
        -- run day3 "../2022/day3.txt"
        -- run day4 "../2022/day4.txt"
        -- run day5 "../2022/day5.txt"
        -- run day6 "../2022/day6.txt"
        run day7 "../2022/day7.txt"