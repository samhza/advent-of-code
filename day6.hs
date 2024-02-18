import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.List
import System.IO
import Data.Semigroup
import Data.Char as C
import GHC.Float (int2Float)

main = do
  contents <- getContents
  let [((t,d), _)] = readP_to_S input contents
  print $ getProduct $ foldMap (Product . possibleW) (zip t d)
  let kern x = read (x >>= show) :: Int
  print $ possibleW (kern t, kern d)

input = do
    t <- times
    d <- times
    return (t,d)

possibleW (t,d) =
    let
     (first, last) = roots 1 (fromIntegral (-t)) (fromIntegral d)
    in
    (floor first - max 1 (floor last))

                          
roots a b c= (x1, x2)
    where
        x1 = e + sqrt d / (2 * a)
        x2 = e - sqrt d / (2 * a)
        d = b * b - 4 * a * c
        e = - b / (2 * a)
dist t h = h * (t - h)

times :: ReadP [Int]
times = do
    many (satisfy (/=':'))
    char ':'
    skipSpaces
    wtv <- sepBy number (many1 (char ' '))
    char '\n'
    return wtv

number :: ReadP Int
number = read <$> many1 (satisfy C.isDigit)
