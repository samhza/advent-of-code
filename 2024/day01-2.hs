import Control.Arrow
import Control.Monad
import Data.Either
import Data.List
import Data.Map qualified as M
import System.IO
import Text.Parsec
import Data.Maybe

number = read <$> many1 digit
input = unzip <$> many (liftM2 (,) (number <* spaces) number <* newline) <* eof
part1 = sum . map abs . uncurry (zipWith (-)) . (sort *** sort)
part2 (xs, ys) =
  let freq = M.fromListWith (+) $ map (,1) ys
   in sum . mapMaybe (\x -> fmap (x*) (M.lookup x freq)) $ xs

main = getContents >>= print . (part1 &&& part2) . either (error . show) id . parse input "stdin"


