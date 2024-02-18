import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.List
import System.IO
import Data.Ord
import Data.Functor
import Data.Semigroup
import Data.Char as C ( isDigit )
import GHC.Float (int2Float)
import qualified Data.Map as M
import Control.Applicative

data Direction = L | R deriving Show


main = do
  contents <- getContents
  let [((dir,lines), _)] = readP_to_S input contents
  let stuff = M.fromList lines
  print $ fnd (cycle dir) "AAA" (=="ZZZ") stuff 0
  let starts = filter ((== 'A') . head) $ M.keys stuff
  let pathlens = map (\x-> fnd (cycle dir) x ((=='Z') . head) stuff 0) starts
  print $ foldr1 lcm pathlens


fnd (d:dirs) cur pred m n
    | pred cur = n
    | otherwise = fnd dirs next pred m (n+1)
    where
        (l, r) = m M.! cur
        next = case d of
            L -> l
            R -> r

input = do
    dir <- many1 direction
    string "\n\n"
    wtv <- sepBy line (char '\n')
    char '\n'
    eof
    return (dir,wtv)

line = do
    x <- node
    string " = ("
    y <- node
    string ", "
    z <- node
    char ')'
    return (x,(y,z))

direction :: ReadP Direction
direction = char 'L' $> L <|> char 'R' $> R

number :: ReadP Int
number = read <$> many1 (satisfy C.isDigit)

node :: ReadP String
node = do
    a <- get
    b <- get
    c <- get
    return [c,b,a]
