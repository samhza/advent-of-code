import Text.ParserCombinators.ReadP
import Control.Monad
import qualified Data.Char as C
import System.IO
import qualified Data.Set as S
import qualified Data.Map as M

main = do
  contents <- getContents
  let [(cards, _)] = readP_to_S input contents
  putStr "part 1: "
  print $ sum . map score $ cards
  let nmatches = map nMatches cards
  let ans = cardCount nmatches
  putStr "part 2: "
  print $ sum ans

cardCount [] = []
cardCount (n:xs) =
  (1 + sum (take n rem)) : rem
  where rem = cardCount xs

nMatches (win, have) = S.size $ S.intersection (S.fromList win) (S.fromList have)

score x
  | n == 0 = 0
  | otherwise = 2^(n-1)
  where n = nMatches x

input :: ReadP [([Int], [Int])]
input = do
  result <- sepBy card (char '\n')
  char '\n'
  eof
  return result

card = do
  string "Card"
  spaces
  num <- number
  char ':'
  spaces
  win <- sepBy number spaces
  spaces
  char '|'
  spaces
  have <- sepBy number spaces
  return (win, have)

number = read <$> many1 (satisfy C.isDigit)

spaces = void (many1 (char ' '))
