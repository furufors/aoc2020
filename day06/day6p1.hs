#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Data.List.Split

main :: IO ()
main = interact $ show . sum . map (length . nub . sort . concat) . splitOn [""] . lines