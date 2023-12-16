import System.IO
import Data.Either
import Data.List
import qualified Data.Text as T
import Data.Text.Read (decimal)

parseInput s x y
  | T.null s = []
  | Right (n,rem) <- decimal s = Right (n,x,y, T.length s - T.length rem) : parseInput rem (x + T.length s - T.length rem) y
  | T.head s == '\n' = parseInput (T.tail s) 0 (y+1)
  | (dots, rem) <- T.span (=='.') s, not (T.null dots) = parseInput rem (x + T.length dots) y
  | otherwise = Left (T.head s, x, y) : parseInput (T.tail s) (x+1) y

main = do
  contents <- getContents
  let ctext = T.pack contents
  let (sym, num) = partitionEithers $ parseInput ctext 0 0
  let parts = [ (x, y, s, n)
              | (s, x, y) <- sym
              , (n, x', y', len) <- num
              , abs (y-y') <= 1
              , (x+1) >= x' && x <= (x'+len)
              ]
  putStr "part 1: "
  print $ sum . map (\(_,_,_,n)->n) $ parts
  putStr "part 2: "
  print $ sum . map (product . map snd)
              . filter ((==2) . length)
              . groupBy (\(p,_) (p',_)->p==p')
              $ [((x,y),n) | (x,y,'*',n) <- parts]
