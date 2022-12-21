{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day11
  ( day11,
  )
where

import Text.Parsec
import Text.Parsec.String
import Data.Sort

day11 :: String -> IO ()
day11 inp = do
  let c = parseInput inp
  case c of
    Right a -> do
      print $ part1 a
      -- print $ part2 a
    Left b -> print b

-- part1 :: [Monkey] -> [Monkey]
part1 xs = product . take 2 . reverse . sort . map getCounter $ foldr (flip throwItems) xs (concat $ replicate 20 [length xs - 1,length xs - 2..0])

-- part2 xs = xs

getCounter :: Monkey -> Int
getCounter (Monkey _ c _ _ _ _ ) = c


throwItems :: [Monkey] -> Int -> [Monkey]
throwItems monkeys monkeyId
  | length items == 1 = throwItem' monkeys (monkeys !! monkeyId)
  | null items = monkeys
  | otherwise = throwItems (throwItem' monkeys (monkeys !! monkeyId)) monkeyId
  where
    (Monkey _ _ items _ _ _) = monkeys !! monkeyId

throwItem' :: [Monkey] -> Monkey -> [Monkey]
throwItem' monkeys (Monkey x _ (item:_) op (DivisibleBy d) (Throw t f))
  | operatedItem `mod` d == 0 = throwItem monkeys x op t
  | otherwise =                 throwItem monkeys x op f
  where
    operatedItem = applyOperation op item
throwItem' _ _ = error "AAAAAHH"

throwItem :: [Monkey] -> Int -> Operation -> Int -> [Monkey]
throwItem ms from op to = addItem ms' to (applyOperation op i)
  where (i, ms') = removeItem ms from

addItem :: [Monkey] -> Int -> Int -> [Monkey]
addItem ms n i = before ++ [Monkey x' c (items ++ [i]) op tst throw] ++ after
  where (before, (Monkey x' c items op tst throw):after) = break (\(Monkey x _ _ _ _ _) -> x == n) ms

removeItem :: [Monkey] -> Int -> (Int, [Monkey])
removeItem ms n = (head items, before ++ [Monkey x' (c+1) (tail items) op tst throw] ++ after)
  where (before, (Monkey x' c items op tst throw):after) = break (\(Monkey x _ _ _ _ _) -> x == n) ms


applyOperation :: Operation -> Int -> Int
applyOperation (Add x) = flip div 3 . (+x)
applyOperation (Multiply x) = flip div 3 . (*x)
applyOperation AddOld = flip div 3 . (*2)
applyOperation MultiplyOld = flip div 3 . (^2)

-- Define data types for the intermediate results of the parser
data Operation = Multiply Int | Add Int | MultiplyOld | AddOld deriving (Show)
newtype Test = DivisibleBy Int deriving (Show)
data Throw = Throw Int Int deriving (Show)
data Monkey = Monkey Int Int [Int] Operation Test Throw deriving (Show)

-- Define a parser for integers
intParser :: Parsec String () Int
intParser = read <$> many1 digit

-- Define a parser for monkey starting items
startingItemsParser :: Parsec String () [Int]
startingItemsParser = do
  _ <- string "Starting items: "
  intParser `sepBy` string ", "

-- Define a parser for the operation
operationParser :: Parser Operation
operationParser = do
  _ <- string "Operation: new = old "
  op <- many1 (noneOf " \n")
  case op of
    "*" -> try multParser <|> multiplyOldParser
    "+" -> try addParser <|> addOldParser
    _ -> fail ("|" ++ op ++ "|")
  where
    multParser = Multiply <$> (string " " *> intParser)
    addParser = Add <$> (string " " *> intParser)
    addOldParser = AddOld <$ string " old"
    multiplyOldParser = MultiplyOld <$ string " old"

-- Define a parser for the monkey test
testParser :: Parsec String () Test
testParser = do
  _ <- string "Test: divisible by "
  DivisibleBy <$> intParser

-- Define a parser for the monkey throw
throwParser :: Parsec String () Throw
throwParser = do
  _ <- string "If true: throw to monkey "
  num <- intParser
  _ <- string "\n    If false: throw to monkey "
  Throw num <$> intParser

-- Define a parser for a monkey
monkeyParser :: Parsec String () Monkey
monkeyParser = do
  _ <- string "Monkey "
  num <- intParser
  _ <- string ":\n  "
  startingItems <- startingItemsParser
  _ <- string "\n  "
  operation <- operationParser
  _ <- string "\n  "
  test <- testParser
  _ <- string "\n    "
  Monkey num 0 startingItems operation test <$> throwParser

-- Define a parser for the entire input
inputParser :: Parsec String () [Monkey]
inputParser = monkeyParser `sepEndBy` string "\n\n"

-- Parse an input string and return the result
parseInput :: String -> Either ParseError [Monkey]
parseInput = parse inputParser "error parsing"