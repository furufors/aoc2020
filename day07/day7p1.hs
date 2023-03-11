#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.List
type Bag = String
type BagCount = (Bag, Integer)
type Rule = (Bag, [BagCount])

main :: IO ()
main = interact $ show . solve . map parsein . lines

solve :: [Rule] -> Int
solve rs = length . filter (/= "shiny gold") $ expand ["shiny gold"] rs

expand :: [Bag] -> [Rule] -> [Bag]
expand bs rs = if step bs rs == bs
               then bs
               else expand (step bs rs) rs

step :: [Bag] -> [Rule] -> [Bag]
step bs rs = nub $ bs ++ [ c | b <- bs, (c,cs) <- rs, (d,_) <- cs, b == d]

parsein :: String -> Rule
parsein input = case parse parseRule "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseRule :: Parsec String () Rule
parseRule = do -- light red bags contain 1 bright white bag, 2 muted yellow bags.
    color  <- parseColor
    ______ <- string " bags contain "
    bags   <- choice [try noOtherBags, try bagList]
    ______ <- string "."
    return $ (color, bags)

noOtherBags = string "no other bags" >> return []
bagList = (parseBagCount `sepBy` (string ", "))

parseBagCount :: Parsec String () BagCount
parseBagCount = do -- 1 bright white bag or 2 muted yellow bags
    count  <- read <$> many1 digit
    ______ <- string " "
    color  <- parseColor
    ______ <- choice [try $ string " bags", try $ string " bag"]
    return $ (color, count)

parseColor :: Parsec String () String
parseColor = do
    adjective <- many1 lower
    _________ <- string " "
    color     <- many1 lower
    return $ concat [adjective, " ", color]
