module Main (main) where

-- import Day1
-- import Day2
-- import Day3
-- import Day4
-- import Day5
-- import Day6
-- import Day7
-- import Day8
-- import Day9
-- import Day10
import Day11

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
        -- run day7 "../2022/day7.txt"
        -- run day8 "../2022/day8.txt"
        -- run day9 "../2022/day9.txt"
        -- run day10 "../2022/day10.txt"
        run day11 "../2022/day11t.txt"