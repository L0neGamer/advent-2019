module Useful where
import System.CPUTime
import qualified System.IO
import Data.Maybe
import Data.Either
import qualified Data.Map as M
import Data.List

type Point = (Integer, Integer)
type TileType = Integer
type Tile = (TileType, Point)
type TileMap = M.Map Point Tile
data Bearing = N | S | E | W

getBearing :: Point -> Point -> Bearing
getBearing (x, y) (x', y')
  | abs xDif > abs yDif && xDif > 0 = E
  | abs xDif > abs yDif && xDif < 0 = W
  | yDif < 0 = S
  | yDif > 0 = N
  where xDif = x' - x
        yDif = y' - y

moveInBearing :: Point -> Bearing -> Point
moveInBearing (x, y) N = (x, y+1)
moveInBearing (x, y) E = (x+1, y)
moveInBearing (x, y) W = (x-1, y)
moveInBearing (x, y) S = (x, y-1)

splitOn'' :: Char -> [String] -> [String]
splitOn'' c [] = [[c]]
splitOn'' c ([]:xs) = [c]:xs
splitOn'' c (x:xs) = (c:x):xs

splitOn :: String -> String -> [String]
splitOn _ [] = []
splitOn [] (s:str) = [s] : splitOn [] str
splitOn splitStr (s:str)
  | splitStrLen > length str = [s:str]
  | take splitStrLen (s:str) == splitStr = "" : splitOn splitStr (drop splitStrLen (s:str))
  | otherwise = splitOn'' s $ splitOn splitStr str
  where splitStrLen = length splitStr


---- thanks to https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

timeDif :: Integer -> IO Integer
timeDif x = do
             y <- getCPUTime
             print $ (fromIntegral (y-x))/(10^9)
             return y

fromStr :: String -> Char -> [String]
fromStr str c = wordsWhen (==c) str

enumerate :: [a] -> [(Integer, a)]
enumerate xs = enumerate' xs 0

enumerate' :: [a] -> Integer -> [(Integer, a)]
enumerate' [] _ = []
enumerate' (x:xs) i = (i, x):enumerate' xs (i+1)

replaceAt n xs x = fst splitLst ++ [x] ++ (tail $ snd splitLst)
  where splitLst = splitAt (fromInteger n) xs

readFile str = System.IO.readFile str

prime_factors :: Integer -> [Integer]
prime_factors 1 = []
prime_factors i = divisor:next
  where divisor = prime_factors' i primes
        next = prime_factors (div i divisor)
prime_factors' :: Integer -> [Integer] -> Integer
prime_factors' i (p:ps)
  | rem i p == 0 = p
  | otherwise = prime_factors' i ps

primes = 2 : primes'
  where isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
        primes' = 3 : filter (isPrime primes') [5, 7 ..]

notEmpty :: [[a]] -> [[a]]
notEmpty [] = []
notEmpty ([]:xs) = notEmpty xs
notEmpty (x:xs) = x:notEmpty xs

compInt :: Integer -> Integer -> Integer
compInt x x'
  | x > x' = -1
  | x < x' = 1
  | x == x' = 0

fromRight (Right a) = a
fromLeft (Left a) = a

callBinarySearch :: (Integral a, Ord b) => b -> (a -> b) -> a
callBinarySearch aim f = callBinarySearch' aim 1 10 f

callBinarySearch' :: (Integral a, Ord b) => b -> a -> a -> (a -> b) -> a
callBinarySearch' aim lower upper f
  | isRight searched = fromRight searched
  | otherwise = res f
  where searched = binarySearch aim lower upper f
        leftVal = fromLeft searched
        res | leftVal == 0  = callBinarySearch' aim (upper - 1) (lower + 1)
            | leftVal == -1 = callBinarySearch' aim (lower - 1 - (abs lower)) upper
            | leftVal == 1  = callBinarySearch' aim lower (upper + 1 + (abs upper))

binarySearch :: (Integral a, Ord b) => b -> a -> a -> (a -> b) -> Either Int a
binarySearch aim lower upper f
  | lower >= upper = Left 0
  | aim > upperVal = Left 1
  | aim < lowerVal = Left (-1)
  | pivotVal == aim = return pivot
  | upperVal == aim = return upper
  | lowerVal == aim = return lower
  | pivot == lower && upperVal > aim = return pivot
  | aim > pivotVal = binarySearch aim pivot upper f
  | aim < pivotVal = binarySearch aim lower pivot f
  where lowerVal = f lower
        upperVal = f upper
        pivot = div (lower+upper) 2
        pivotVal = f pivot

drawInt :: Integer -> Char
drawInt 0 = ' '
drawInt 1 = head "\x2588"
drawInt 2 = 'o'
drawInt 3 = 'O'

drawTileMap :: TileMap -> String
drawTileMap pm = concat [ getRow y | y <- [max_y,(max_y-1)..min_y]]
  where keys = M.keys pm :: [Point]
        tup_cmp fnc a b = compare (fnc a) (fnc b)
        getMostVal cmp fnc ks = fnc $ cmp (tup_cmp fnc) ks
        max_x = getMostVal maximumBy fst keys
        min_x = getMostVal minimumBy fst keys
        max_y = getMostVal maximumBy snd keys
        min_y = getMostVal minimumBy snd keys
        getRow y = [drawInt (fst (M.findWithDefault (0,(0,0)) (x,y) pm)) | x <- [min_x..max_x]] ++ "\n" :: String

manhattan (x,y) (x',y') = abs (x - x') + abs (y - y')

dirtyGetPath :: Point -> TileMap -> [Point]
dirtyGetPath (0,0) _ = []
dirtyGetPath p tm = p : dirtyGetPath (snd $ tm M.! p) tm
