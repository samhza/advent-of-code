import Text.ParserCombinators.ReadP
import Data.List
import Data.Ord
import Data.Semigroup
import Data.Char as C ( isDigit )
import qualified Data.Map as M

data HandType = High | One | Two | Three | Full | Four | Five deriving (Eq, Ord, Enum, Show)

cardOrder = "23456789TJQKA"
compareCard :: Char -> Char -> Ordering
compareCard = comparing (`elemIndex` cardOrder)

compareHands :: String -> String -> Ordering
compareHands a b =
    comparing handType a b <> foldMap (uncurry compareCard) (zip a b)

handType :: String -> HandType
handType s =
    let counts = M.fromListWith (+)  (map (, 1) s)
    in
        case sortOn Down (M.elems counts) of
            [5] -> Five
            4:_ -> Four
            3:2:_ -> Full
            3:_ -> Three
            2:2:_ -> Two
            2:_ -> One
            _ -> High

main = do
  contents <- getContents
  let [(lines, _)] = readP_to_S input contents
  let sorted = sortBy (\(a,_) (b,_) -> compareHands a b) lines
  print $ sum $ zipWith (\(_,b) r -> r*b) sorted [1..]

input = do
    wtv <- sepBy line (char '\n')
    char '\n'
    eof
    return wtv

line = do
    card <- count 5 get
    char ' '
    bid <- number
    return (card, bid)

number :: ReadP Int
number = read <$> many1 (satisfy C.isDigit)
