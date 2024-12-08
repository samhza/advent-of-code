import Text.ParserCombinators.ReadP
import qualified Data.Char as C
import System.IO
import Data.Semigroup

main = do
  contents <- getContents
  let [((seeds, maps), [])] = readP_to_S input contents
  let locations = foldMap (\s-> Min $ foldl resolve s maps) seeds
  let ts = pair seeds
  print locations
  print ts
  let wtv = foldl (\inn ranges-> inn >>= (`applyMap` ranges)) ts maps
  print $ foldMap (Min . fst) wtv

type Transform = (Int, Int, Int)
type Map = [Transform]
type Range = (Int, Int)

pair :: [Int] -> [Range]
pair [] = []
pair (x:y:xs) = (x,y):pair xs

resolve :: Int -> Map -> Int
resolve a [] = a
resolve n ((dest, src, len):xs)
  | n < src = resolve n xs
  | n > src + len = resolve n xs
  | otherwise = n - src + dest

applyMap :: Range -> Map -> [Range]
applyMap r [] = [r]
applyMap r (t:ts) =
  let (leftovers, out) = intersection r t in
    out ++ (leftovers >>= (`applyMap` ts))

intersection :: Range -> Transform -> ([Range], [Range])
intersection s@(start, length) (dest, src, len)
  | (not . valid) overlap = ([s], [])
  | otherwise = (leftovers, [overlap])
  where
    pre = (start, src - start)
    post = (src + len, start + length - src - len)
    valid = (>0) . snd
    leftovers = filter valid [pre,post]
    st = max start src 
    overlap = (st - src + dest, min (src + len) (start + length) - st)

input = do
  string "seeds: "
  seeds <- sepBy number (char ' ')
  string "\n\n"
  ret <- sepBy parseMap (char '\n')
  eof
  return (seeds, ret)

parseMap :: ReadP [(Int, Int, Int)]
parseMap = do
  skipMany (satisfy (/='-'))
  string "-to-"
  name <- many (satisfy (/=' '))
  string " map:\n"
  ranges <- sepBy range (char '\n')
  char '\n'
  return ranges

range :: ReadP (Int, Int, Int)
range = do
  dest <- number
  char ' '
  src <- number
  char ' '
  len <- number
  return (dest, src, len)

number :: ReadP Int
number = read <$> many1 (satisfy C.isDigit)
