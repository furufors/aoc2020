#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
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
        candidates :: Int -> Int -> [(Int,Int)]
        candidates x y = filter (inbounds width height) [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
        neighbours :: Int -> Int -> [Square]
        neighbours x y = map (\(x,y) -> room!!y!!x) $ candidates x y
        step :: Room -> Room
        step room = [[ rule (room!!y!!x) (neighbours x y) | x <- [0..(width-1)] ] | y <- [0..(height-1)]]
    in if step room == room
       then countTaken room
       else solve (step room)

countTaken :: Room -> Int
countTaken = sum . map sum . map (map isTaken)
    where
        isTaken Taken = 1
        isTaken _____ = 0

rule :: Square -> [Square] -> Square
rule Floor _ = Floor
rule Taken ns | length (filter (== Taken) ns) >= 4 = Free
rule Free  ns | length (filter (== Taken) ns) == 0 = Taken
rule x     _ = x

parse :: String -> Line
parse = map toSquare

toSquare :: Char -> Square
toSquare '.' = Floor
toSquare 'L' = Free
toSquare ___ = Taken
