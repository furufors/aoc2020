#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.List
import Data.Maybe
import Data.Ord

main :: IO ()
main = interact $ show . solve . parseLine . last . lines

solve :: [(Int, Int)] -> Int
solve ps = solve' 1 (head ps) (tail ps)
    where
        solve' n (b, o) ps =
            let time = n * 13 * b - o
            in if all (departAt time) ps
               then time
               else solve' (n+1) (b, o) ps
        departAt i (b, o) = (i+o) `mod` b == 0

parseLine :: String -> [(Int, Int)]
parseLine ls = reverse . sortBy (comparing fst) . filterJusts $ zip (parseBusses ls) [0..]

filterJusts :: [(Maybe Int, Int)] -> [(Int, Int)]
filterJusts ((Just i,j)  :ps) = (i,j):(filterJusts ps)
filterJusts ((Nothing, j):ps) = filterJusts ps
filterJusts [               ] = []

parseBusses :: String -> [Maybe Int]
parseBusses input = case parse parseBusses2 "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseBusses2 :: Parsec String () [Maybe Int]
parseBusses2 = do
    busses  <- (try busNumber <|> try busX) `sepBy` (string ",")
    return $ busses

busNumber :: Parsec String () (Maybe Int)
busNumber = Just . read <$> many1 digit

busX :: Parsec String () (Maybe Int)
busX = do
    _ <- string "x"
    return Nothing