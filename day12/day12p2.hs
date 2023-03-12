#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.List
data Action = N | E | S | W | L | R | F deriving (Eq, Show)
type Instruction = (Action, Int)
type Position = ((Int, Int), WayPoint)
type WayPoint = (Int, Int)

main :: IO ()
main = interact $ show . manhattan . foldl followInstruction startPos . map parsein . lines
    where
        startPos = ((0, 0), (10,1))

manhattan :: Position -> Int
manhattan ((x,y),_) = abs x + abs y

followInstruction :: Position -> Instruction -> Position
followInstruction pos (N,i) = moveWp ( 0,  i) pos
followInstruction pos (E,i) = moveWp ( i,  0) pos
followInstruction pos (S,i) = moveWp ( 0, -i) pos
followInstruction pos (W,i) = moveWp (-i,  0) pos
followInstruction pos@((x,y),(wx,wy)) (L,  90) = ((x,y),(-wy, wx))
followInstruction pos@((x,y),(wx,wy)) (L, 180) = ((x,y),(-wx,-wy))
followInstruction pos@((x,y),(wx,wy)) (L, 270) = ((x,y),( wy,-wx))
followInstruction pos@((x,y),(wx,wy)) (R,  90) = ((x,y),( wy,-wx))
followInstruction pos@((x,y),(wx,wy)) (R, 180) = ((x,y),(-wx,-wy))
followInstruction pos@((x,y),(wx,wy)) (R, 270) = ((x,y),(-wy, wx))
followInstruction pos@((x,y),(wx,wy)) (F, i) = move (i*wx,i*wy) pos

move :: (Int, Int) -> Position -> Position
move (a, b) ((x, y), (wx,wy)) = ((x+a, y+b), (wx, wy))

moveWp :: (Int, Int) -> Position -> Position
moveWp (a, b) ((x, y), (wx,wy)) = ((x, y), (wx+a, wy+b))

parsein :: String -> Instruction
parsein input = case parse parseInstruction "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseInstruction :: Parsec String () Instruction
parseInstruction = do
    action <- parseAction
    count  <- read <$> many1 digit
    return (action, count)

parseAction :: Parsec String () Action
parseAction = toAction <$> upper
    where
        toAction 'N' = N
        toAction 'E' = E
        toAction 'S' = S
        toAction 'W' = W
        toAction 'L' = L
        toAction 'R' = R
        toAction 'F' = F
        toAction ___ = error "unkown action in parseAction"