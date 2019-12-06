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

extractEither = either id id

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

main = do
        contents <- readFile "input.txt"
        let (orbits, planets) = fromStr contents
            total = runGetOrbits planets orbits
        print total
        print $ length $ findRoute "YOU" "SAN" orbits

findRoute :: String -> String -> Map String String -> [String]
findRoute x y orbits = tail $ findRoute' xOrbits yOrbits
  where xOrbits = reverse $ tail $ getOrbits x orbits
        yOrbits = reverse $ tail $ getOrbits y orbits

findRoute' :: [String] -> [String] -> [String]
findRoute' [] [] = []
findRoute' (x:x':xs) (y:y':ys)
  | x == y && x' == y' = findRoute' (x':xs) (y':ys)
  | x == y && x' /= y' = reverse (x':xs) ++ (y:y':ys)
  | otherwise = error "no state"

runGetOrbits :: [String] -> Map String String -> Int
runGetOrbits xs orbits = sum $ map (\x -> length $ getOrbits x orbits) xs

getOrbits :: String -> Map String String -> [String]
getOrbits "COM" _ = []
getOrbits x xs = x: (getOrbits (xs Map.! x) xs)

fromStr :: String -> (Map String String, [String])
fromStr str = (Map.fromList pairs, map (\(x,_) -> x) pairs)
  where lineLst = wordsWhen (=='\n') str
        pairs = map (\(x:y:xs) -> (y,x)) (map (wordsWhen (==')')) lineLst)

timeDif :: Integer -> IO Integer
timeDif x = do
             y <- getCPUTime
             print $ (fromIntegral (y-x))/(10^9)
             return y

--sortedVals :: Int -> Int -> [String]
--sortedVals bot top = filter (\a -> a == sort a) $ map show [bot..top]
--countChar str c = length $ filter (== c) str
--anyMatch opt str = any (\c -> opt (countChar str c)) str
----anyMatch opt str = or $ map (\c -> opt (countChar str c)) str
--res1 bot top = filter (\str -> any (\c -> (>=2) $ countChar str c) str) $ sortedVals bot top
--res2 bot top = filter (\str -> any (\c -> (==2) $ countChar str c) str) $ sortedVals bot top
