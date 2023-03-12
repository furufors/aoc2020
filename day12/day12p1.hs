#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.List
data Facing = FN | FE | FS | FW deriving (Eq, Show)
data Action = N | E | S | W | L | R | F deriving (Eq, Show)
type Instruction = (Action, Int)
type Position = (Facing, (Int, Int))

main :: IO ()
main = interact $ show . manhattan . foldl followInstruction startPos . map parsein . lines
    where
        startPos = (FE, (0, 0))

manhattan :: Position -> Int
manhattan (_,(x,y)) = abs x + abs y

followInstruction :: Position -> Instruction -> Position
followInstruction pos (N,i) = move ( 0,  i) pos
followInstruction pos (E,i) = move ( i,  0) pos
followInstruction pos (S,i) = move ( 0, -i) pos
followInstruction pos (W,i) = move (-i,  0) pos
followInstruction pos (L,  90) = (rotL) pos
followInstruction pos (L, 180) = (rotL . rotL) pos
followInstruction pos (L, 270) = (rotL . rotL . rotL) pos
followInstruction pos (R,  90) = (rotR) pos
followInstruction pos (R, 180) = (rotR . rotR) pos
followInstruction pos (R, 270) = (rotR . rotR . rotR) pos
followInstruction (f,c) (F, i) = followInstruction (f,c) (toAction f,i)

move :: (Int, Int) -> Position -> Position
move (a, b) (f, (x, y)) = (f, (x+a, y+b))

rotL :: Position -> Position
rotL (FN,i) = (FW,i)
rotL (FE,i) = (FN,i)
rotL (FS,i) = (FE,i)
rotL (FW,i) = (FS,i)

rotR :: Position -> Position
rotR (FN,i) = (FE,i)
rotR (FE,i) = (FS,i)
rotR (FS,i) = (FW,i)
rotR (FW,i) = (FN,i)

toAction :: Facing -> Action
toAction FN = N
toAction FE = E
toAction FS = S
toAction FW = W

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