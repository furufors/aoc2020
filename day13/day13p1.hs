#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.List
import Data.Ord

main :: IO ()
main = interact $ show . (\(b,t) -> b*t) . head . sortBy (comparing snd) . toWait . parseLines . lines

toWait :: (Int, [Int]) -> [(Int, Int)]
toWait (t,bs) = map (\b -> (b, b - (t `mod` b))) bs

parseLines :: [String] -> (Int, [Int])
parseLines ls = (read . head $ ls, parseBusses . last $ ls)

parseBusses :: String -> [Int]
parseBusses input = case parse parseBusses2 "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseBusses2 :: Parsec String () [Int]
parseBusses2 = do
    busses  <- (try busNumber <|> try busX) `sepBy` (string ",")
    return $ concat busses

busNumber :: Parsec String () [Int]
busNumber = (:[]) . read <$> many1 digit

busX :: Parsec String () [Int]
busX = do
    _ <- string "x"
    return []