import Data.List
import Text.Parsec
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

number = read <$> many1 digit
rule = (,) <$> number <* char '|' <*> number
input = (,) 
    <$> rule `endBy` newline <* newline
    <*> (number `sepBy` char ',') `endBy` newline

solution (rules, manuals) = 
    let deps = buildDeps rules
        part1 = sum . map middle . filter (isSorted deps) $ manuals
        invalid = filter (not . isSorted deps) manuals
        sorted = map (sortBy (orderPages deps)) invalid
        part2 = sum . map middle $ sorted
    in (part1, part2)

middle x = let x' = Seq.fromList x in Seq.index x' (Seq.length x' `div` 2)
buildDeps = foldr (\(x,y) -> M.insertWith S.union y (S.singleton x)) M.empty
orderPages deps a b
  | a `dependsOn` b = GT 
  | b `dependsOn` a = LT
  | otherwise = EQ
  where dependsOn a b = S.member b (M.findWithDefault S.empty a deps) 
isSorted deps xs = and $ zipWith (\a b -> orderPages deps a b /= GT) xs (tail xs)

main = getContents >>= print . solution . either (error . show) id . parse input "stdin"

