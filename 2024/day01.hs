import System.IO
import Text.Parsec
import Control.Monad
import Control.Arrow
import Data.Either
import Data.List

number = read <$> many1 digit
input = many (liftM2 (,) (number <* spaces) number <* newline) <* eof
solution = sum . map abs . uncurry (zipWith (-)) . (sort *** sort) . unzip
main = getContents >>= print . solution . either (error.show) id . parse input ""

