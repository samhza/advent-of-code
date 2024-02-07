import System.IO
import Data.Maybe
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Char as C
import Text.ParserCombinators.ReadP as P

data Color = Red | Green | Blue deriving (Show, Eq, Ord)
type Set = M.Map Color Int

main = do
  contents <- getContents
  let (games, _):_ = readP_to_S parseFile contents
  putStr "part 1: "
  print $ sum . map fst . filter (possible . snd) $ games
  putStr "part 2: "
  print $ sum . map (product . snd) $ games
  return ()

parseFile = do
  result <- many1 parseLine
  eof
  return result

parseLine = do
  string "Game "
  n <- number
  string ": "
  game <- sepBy parseRound (string "; ")
  char '\n'
  return (n, M.fromListWith max . fold $ game)

possible set
  | set M.! Red > 12 = False
  | set M.! Green > 13 = False
  | set M.! Blue > 14 = False
  | otherwise = True

parseRound = sepBy hand (string ", ")

hand = do
  n <- number
  char ' '
  c <- color
  return (c,n)

space = char ' '

number = read <$> munch C.isDigit

color = do
  choice
    [ string "red" >> pure Red
    , string "green" >> pure Green
    , string "blue" >> pure Blue
    ]
