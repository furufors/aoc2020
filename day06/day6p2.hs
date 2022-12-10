#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Ord
import Data.List
import Data.List.Split

main :: IO ()
main = interact $ show . sum . map (check . sortBy (comparing length)) . splitOn [""] . lines

check :: [String] -> Int
check (s:ss) = length . filter id . map (all id) $ [[ l `elem` e | e <- ss] | l <- s]