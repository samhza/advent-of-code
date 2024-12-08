import qualified Data.Set as S
import qualified Data.ByteString.Char8 as T
import Data.Maybe ( fromJust )
import Data.Tuple (swap)
import Data.List (sortBy, groupBy, sort)
import GHC.Exts (groupWith)

data Tile = Ground | Start | Pipe (Direction, Direction) deriving (Eq, Show)
type Grid = (T.ByteString, Int) -- contents, width
type Coord = (Int, Int)
type Direction = (Int, Int)

n = (0,-1)
s = (0,1)
e = (1, 0)
w = (-1,0)

inv (x,y) = (-x,-y)

tile '.' = Ground
tile '|' = Pipe (n,s)
tile '-' = Pipe (e,w)
tile 'J' = Pipe (n,w)
tile 'L' = Pipe (n,e)
tile 'F' = Pipe (s,e)
tile '7' = Pipe (s,w)
tile 'S' = Start

grid :: T.ByteString -> Grid
grid = (,) <*> fromJust . T.findIndex (=='\n')

tileAt :: Grid -> Coord -> Tile
tileAt (v, width) (x,y) =
    tile . T.index v $ x + (y * (width + 1))

startPos :: Grid -> Coord
startPos (text, w) =
    (swap . (`divMod` (w+1)) . fromJust) $ T.findIndex (=='S') text

move :: Coord -> Direction -> Coord
move (x,y) (dx,dy) = (x+dx,y+dy)

connected :: Grid -> Coord -> ((Direction, Coord), (Direction, Coord))
connected g pos =
    let [a,b] = [ (dir, pos')
                | dir <- [n,s,e,w]
                , let pos'@(x,y) = move pos dir
                , x >= 0 && y >= 0
                , case tileAt g pos' of
                    Pipe(d1,d2) -> d1 == inv dir || d2 == inv dir
                    _ -> False
                ]
    in (a,b)
-- looplength :: Num t => Grid -> Direction -> Coord -> t -> t
loop g from pos
    | Start <- tile = [pos]
    | otherwise = pos : loop g dir (move pos dir)
    where
        tile = tileAt g pos
        Pipe (d1,d2) = tile
        dir = if d1 /= inv from then d1 else d2

part2 (g,_) tiles =
    let t = S.fromList tiles
        (_, n, _) = T.foldl' (\(ins, n, (x,y)) c->
            if c == '.' && ins then (ins, n+1, (x+1,y))
            else if S.member (x,y) t then (not ins, n, (x+1,y))
            else if c == '\n' then (False, n, (0,y+1))
            else (ins, n, (x+1,y))
            ) (False, 0, (0,0)) g
        in
            n

-- segments x:y:xs = (x,y): segments xs
-- segments _ = []

main = do
    contents <- T.getContents
    let g = grid contents
    let ((from, pos),_) = connected g (startPos g)
    let tiles = loop g from pos
    print . ceiling . (/2) $ fromIntegral $ length tiles
    --print $ part2 g tiles
    print ":3"

