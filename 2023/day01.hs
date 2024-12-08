import System.IO
import Data.List as L
import Data.Maybe
import Data.Monoid
import qualified Data.Char as C

main = do
  content <- getContents
  print $ sum . mapMaybe (calib . digs) . lines $ content

numbers =  ["one","two","three","four","five","six","seven","eight","nine"]

calib (First x, Last y) = (+) . (*10) <$> x <*> y

digs = foldMap (((,) . First <*> Last) . parseDig) . tails

parseDig :: String -> Maybe Int
parseDig [] = Nothing
parseDig s@(fs:_)
  | C.isDigit fs = Just $ read [fs]
  | otherwise = fmap fst . find (\(_,n) -> n `isPrefixOf` s) . zip [1..] $ numbers
