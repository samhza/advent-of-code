import Text.ParserCombinators.ReadP
import Data.Char as C ( isDigit )
import Data.Semigroup

main = do
    contents <- getContents
    let [(ns, _)] = readP_to_S input contents
    print $ getSum $ foldMap (Sum . next) ns
    print $ getSum $ foldMap (Sum . next . reverse) ns

next [x,y] | x == y = x
next xs = last xs + next (diff xs)
    where
        diff (x:y:xs) = y - x : diff (y:xs)
        diff _ = []

input =
    sepBy line (char '\n') <*
    char '\n' <*
    eof

line = sepBy number (char ' ')

number :: ReadP Int
number = read <$> many1 (satisfy (\x-> x=='-' || C.isDigit x))
