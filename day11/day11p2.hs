#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
data Square = Floor | Free | Taken deriving (Show,Eq)
type Line = [Square]
type Room = [Line]

main :: IO ()
main = interact $ show . solve . map parse . lines

solve :: Room -> Int
solve room =
    let width  = length (head room)
        height = length room
        inbounds x y (x',y') = 0 <= x' && x' < x && 0 <= y' && y' < y
        valid = inbounds width height
        anyTaken :: [Square] -> Int
        anyTaken ss = if Taken `elem` ss then 1 else 0
        -- IncludesTaken checks if a taken chair occurs closer than a free chair in the series of squares
        includesTaken :: [(Int,Int)] -> Int
        includesTaken = anyTaken . takeWhile (/=Free) . map (\(x,y) -> room!!y!!x) . takeWhile valid
        sumVisible x y = upleft x y + up x y + upright x y + right x y + downright x y + down x y + downleft x y + left x y
        upleft x y = includesTaken [(x-n, y-n) | n <- [1..]]
        up x y = includesTaken [(x, y-n) | n <- [1..]]
        upright x y = includesTaken [(x+n, y-n) | n <- [1..]]
        right x y  = includesTaken [(x+n, y) | n <- [1..]]
        downright x y  = includesTaken [(x+n, y+n) | n <- [1..]]
        down x y = includesTaken [(x, y+n) | n <- [1..]]
        downleft x y = includesTaken [(x-n, y+n) | n <- [1..]]
        left x y  = includesTaken [(x-n, y) | n <- [1..]]
        step :: Room -> Room
        step room = [[ rule (room!!y!!x) (sumVisible x y) | x <- [0..(width-1)] ] | y <- [0..(height-1)]]
    in if step room == room
       then countTaken room
       else solve (step room)

countTaken :: Room -> Int
countTaken = sum . map sum . map (map isTaken)
    where
        isTaken Taken = 1
        isTaken _____ = 0

rule :: Square -> Int -> Square
rule Floor _ = Floor
rule Taken n | n >= 5 = Free
rule Free  n | n == 0 = Taken
rule x     _ = x

parse :: String -> Line
parse = map toSquare

toSquare :: Char -> Square
toSquare '.' = Floor
toSquare 'L' = Free
toSquare ___ = Taken
