#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . foldl1 (*) . rides . lines

rides :: [String] -> [Int]
rides ss = [ride 0 0 dx dy ss | (dx,dy) <- [(1,1),(3,1),(5,1),(7,1),(1,2)]]

ride :: Int -> Int -> Int -> Int -> [String] -> Int
ride x y dx dy ss = let x' = x `mod` (length $ head ss)
                    in if y >= length ss
                       then 0
                       else (if ss!!y!!x' == '.' then 0 else 1) + ride (x+dx) (y+dy) dx dy ss