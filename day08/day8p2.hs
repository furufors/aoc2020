#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Data.Maybe
import Text.Parsec
data Inst = Nop Int | Acc Int | Jmp Int deriving (Show, Eq)
type Pointer = Int

main :: IO ()
main = interact $ show . runAll . map parsein . lines

runAll ins = head . catMaybes . map (run 0 0 []) . nub $ [(modify i ins) | i <- [0..(length ins - 1)] ]

modify i ins = take i ins ++ [switch (ins!!i)] ++ drop (i+1) ins

switch (Nop n) = Jmp n
switch (Acc n) = Acc n
switch (Jmp n) = Nop n

run :: Pointer -> Int -> [Pointer] -> [Inst] -> Maybe Int
run p acc ps ins =
    if p `elem` ps
    then Nothing
    else if p == length ins
         then Just acc
         else case ins!!p of
              (Nop n) -> run (p + 1) acc (p:ps) ins
              (Acc n) -> run (p + 1) (acc + n) (p:ps) ins
              (Jmp n) -> run (p + n) acc (p:ps) ins

parsein :: String -> Inst
parsein input = case parse (try parseNop <|> try parseAcc <|> try parseJmp) "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseNop :: Parsec String () Inst
parseNop = do
    _ <- string "nop "
    s <- parseSign
    x <- read <$> many1 digit
    return $ Nop (s*x)

parseAcc :: Parsec String () Inst
parseAcc = do
    _ <- string "acc "
    s <- parseSign
    x <- read <$> many1 digit
    return $ Acc (s*x)

parseJmp :: Parsec String () Inst
parseJmp = do
    _ <- string "jmp "
    s <- parseSign
    x <- read <$> many1 digit
    return $ Jmp (s*x)

parseSign = parsePlus <|> parseMinus
parsePlus = string "+" >> return (1)
parseMinus = string "-" >> return (-1)