import System.IO
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Read
import Data.List
import System.CPUTime
import Data.Maybe

splitStr :: String -> Char -> [String]
splitStr str split = wordsWhen (==split) str

---- thanks to https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

removeNothing = catMaybes

--maybeIfy = sequence

extractEither = either id id

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

main = do
--        contents <- readFile "input.txt"
        print "arg"
        print $ length res1
        print $ length res2
--        let
--            wires = removeNothing $ fromInputToWires contents
--            inters = intersections (wires!!0) (wires!!1)
--            lines1 = makeLines $ wires!!0
--            lines2 = makeLines $ wires!!1
----            f dest = (followPath lines1 dest) + (followPath lines2 dest)
----            res = map f inters
--            res = zipWith (+) (map (followPath lines1) inters) (map (followPath lines2) inters)
--        start <- getCPUTime
--        print "Part 1"
--        print $ sol contents
--        middle <- timeDif start
--        print "Part 2"
--        print $ minimum res
--        end   <- timeDif middle
--        print ""

timeDif :: Integer -> IO Integer
timeDif x = do
             y <- getCPUTime
             print $ (fromIntegral (y-x))/(10^12)
             return y

--isValid' :: String -> Char -> Bool -> Bool
--isValid' (x:x':[]) c bool
--  | x' > x = bool
--  | x' == x && x /= c = True
--  | x' == x && x == c = bool
--  | otherwise = False
----isValid' (x:x':x'':[]) c bool
----  | x' > x = isValid' (x':x'':[]) 'a' bool
----  | x' == x && x' /= x'' && x /= c = isValid' (x':x'':[]) x True
----  | otherwise = False
--isValid' (x:x':x'':xs) c bool
--  | x < x' = isValid' (x':x'':xs) 'a' bool
--  | x == c = isValid' (x'':xs) c bool
--  | x' == x && x' == x'' = isValid' (xs) x bool
--  | x' == x && x' /= x'' = isValid' (x'':xs) 'a' True
--  | otherwise = False
--isValid' _ _ bool = bool
--
--isValid :: String -> Bool -> Bool
--isValid str bool = isValid' str 'a' bool
--isValid (x:x':[]) bool
--  | x' > x = bool
--  | x' == x = True
--  | otherwise = False
--isValid (x:x':x'':[]) bool
--  | x' > x = isValid (x':x'':[]) bool
--  | x' == x && x' /= x'' = isValid (x':x'':[]) True
--  | otherwise = False
--isValid (x:x':x'':x''':xs) bool
--  | x' > x = isValid (x':x'':x''':xs) bool
--  | x' == x && x' == x'' && x'' == x''' = isValid (x''':xs) bool
--  | x' == x && x' /= x'' = isValid (x':x'':x''':xs) True
--  | otherwise = False
--isValid _ bool = bool

-- 372304-847060
sortedVals = filter (\a -> a == sort a) $ map show [372304..847060]

countChar str c = length $ filter (== c) str

anyMatch opt str = or $ map (\c -> opt (countChar str c)) str

res1 = filter (anyMatch (>=2)) (sortedVals)
res2 = filter (anyMatch (==2)) (sortedVals)

--result = length $ filter (\a -> isValid a False) (map show [372304..847060])
