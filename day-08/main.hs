import System.IO
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.List
import Data.Maybe
import System.CPUTime

type Image = [Layer]
type Layer = [String]

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

main = do
        contents <- readFile "input.txt"
        contents' <- readFile "testinput.txt"
        let image = intoLayers (toListOfLists contents 25) 6
            res = minimum $ map getDigits $ image
            strForm = toStr $ squashLayers image
            replaced = replace (replace strForm '0' ' ') '1' '#'
        start <- getCPUTime
        print res
        print $ image
        putStrLn replaced
        middle <- timeDif start
        end <- timeDif middle
        print $ ""

countChar str c = length $ filter (== c) str

replace :: String -> Char -> Char -> String
replace [] char rep = []
replace (s:str) char rep
  | s == char = rep:replace str char rep
  | otherwise = s:replace str char rep

toStr :: Layer -> String
toStr [] = ""
toStr (x:xs) = x ++ "\n" ++ (toStr xs)

squashLayers :: Image -> Layer
squashLayers [x] = x
squashLayers (x:y:xs) = squashLayers ((getTop x y):xs)

getTop :: Layer -> Layer -> Layer
getTop x y = zipWith (zipWith getTop') x y
  where getTop' '2' i = i
        getTop' i _ = i

getDigits'' :: [[Int]] -> [Int]
getDigits'' [x] = x
getDigits'' (x:y:xs) = getDigits'' ((zipWith (+) x y):xs)

getDigits' :: String -> [Int]
getDigits' str = map (countChar str) ['0'..'9']

getDigits :: Layer -> [Int]
getDigits row = getDigits'' $ map getDigits' row

intoLayers :: [String] -> Int -> Image
intoLayers [] _ = []
intoLayers xs rows = row: intoLayers rem rows
  where (row, rem) = intoLayers' xs rows

intoLayers' :: [String] -> Int -> (Layer,[String])
intoLayers' xs 0 = ([], xs)
intoLayers' (x:xs) i = (x:row, rem)
  where (row, rem) = intoLayers' xs (i-1)

toListOfLists :: String -> Int -> [String]
toListOfLists [] width = []
toListOfLists inp width = curr : rest
  where (curr, rem) = toListOfLists' inp width
        rest = toListOfLists rem width

toListOfLists' :: String -> Int -> (String, String)
toListOfLists' (x:xs) 1 = ([x], xs)
toListOfLists' (x:xs) width = (x:curr, rem)
  where (curr, rem) = toListOfLists' xs (width-1)
