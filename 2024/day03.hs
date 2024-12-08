import Data.List
import Text.Parsec
import Data.Maybe
import Data.Either
import Data.Functor

number = read <$> many1 digit
mul = Just . Right <$> ((,)
    <$> (string "mul(" *> number)
    <*> (char ',' *> number <* char ')'))
toggle = Just . Left <$>
    (string "do" *>
     (string "()" $> True <|>
      string "n't()" $> False))
input = catMaybes <$> many (try mul <|> try toggle <|> (anyChar $> Nothing))
  
f (_, sum) (Left x) = (x,sum)
f (True, sum) (Right (a,b)) = (True, sum + a*b)
f acc _ = acc

solution input = let
  part1 = sum . map (uncurry (*)) . rights $ input
  (_, part2) = foldl' f (True, 0) input
  in (part1, part2)

main = getContents >>= print . solution . either (error . show) id . parse input "stdin"

