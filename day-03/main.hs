import System.IO
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Read
import Data.List
import System.CPUTime

splitStr :: String -> Char -> [String]
splitStr str split = wordsWhen (==split) str

---- thanks to https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

maybeIfy :: [Maybe a] -> Maybe [a]
maybeIfy [] = Just []
maybeIfy (Nothing:xs) = Nothing
maybeIfy ((Just a):xs) = maybeIfy xs >>= \x -> Just (a:x)

extractEither :: Either a a -> a
extractEither (Left a) = a
extractEither (Right a) = a

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

removeDupes xs = removeDupes' xs []

removeDupes' :: Eq a => [a] -> [a] -> [a]
removeDupes' [] xs = xs
removeDupes' (a:as) xs
  | a `elem` xs = removeDupes' as xs
  | otherwise = removeDupes' as (a:xs)

type Coord = (Int, Int)
data Dir = R | D | L | U deriving (Show, Eq)
type Direction = (Dir, Int)
--data Wire = WireCons Coord Wire | WireEnd deriving (Show, Eq)
data Wire = WireCons [Coord] deriving (Show, Eq)

main = do
        contents <- readFile "input.txt"
        print "arg"
        let
            wires = removeNothing $ fromInputToWires contents
            inters = intersections (wires!!0) (wires!!1)
            lines1 = makeLines $ wires!!0
            lines2 = makeLines $ wires!!1
--            f dest = (followPath lines1 dest) + (followPath lines2 dest)
--            res = map f inters
            res = zipWith (+) (map (followPath lines1) inters) (map (followPath lines2) inters)
        start <- getCPUTime
        print "Part 1"
        print $ sol contents
        middle <- timeDif start
        print "Part 2"
        print $ minimum res
        end   <- timeDif middle
--        end   <- timeDif start
        print ""

makeLines :: Wire -> [[Coord]]
makeLines (WireCons (a:b:xs)) = genCoordLst a b : (makeLines (WireCons (b:xs)))
makeLines _ = []

timeDif :: Integer -> IO Integer
timeDif x = do
             y <- getCPUTime
             print $ (fromIntegral (y-x))/(10^12)
             return y

fromStrLst' :: Dir -> String -> [String] -> [Maybe Direction]
fromStrLst' dir num xs = val : (fromStrLstToDirections xs)
  where valInt = readMaybe num :: Maybe Int
        val = (valInt >>= \a -> Just (dir, a)) :: Maybe Direction

fromStrLstToDirections :: [String] -> [Maybe Direction]
fromStrLstToDirections [] = []
fromStrLstToDirections (('R':num):xs) = fromStrLst' R num xs
fromStrLstToDirections (('U':num):xs) = fromStrLst' U num xs
fromStrLstToDirections (('D':num):xs) = fromStrLst' D num xs
fromStrLstToDirections (('L':num):xs) = fromStrLst' L num xs
fromStrLstToDirections (x:xs) = Nothing:(fromStrLstToDirections xs)

toWire :: [Direction] -> Coord -> Wire
toWire dirs (x,y) = WireCons $ reverse $ foldl toCoord [(x, y)] dirs

toCoord :: [Coord] -> Direction -> [Coord]
toCoord coords dir = (toCoord' dir $ head coords) : coords

toCoord' :: Direction -> Coord -> Coord
toCoord' (R, i) (x, y) = (x + i, y)
toCoord' (L, i) (x, y) = (x - i, y)
toCoord' (U, i) (x, y) = (x, y + i)
toCoord' (D, i) (x, y) = (x, y - i)

fromInputToWires :: String -> [Maybe Wire]
fromInputToWires str = wires
  where lines = splitStr str '\n'
        dirs = map (\a -> maybeIfy (fromStrLstToDirections (splitStr a ','))) lines
        wires = map (\dir -> dir >>= \undir -> Just (toWire undir (0,0))) dirs

sol inpStr = minimum $ map (manhattan (0,0)) inter
  where wires = removeNothing $ fromInputToWires inpStr
        inter = intersections (wires!!0) (wires!!1)

manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

listFromAndTo :: Int -> Int -> [Int]
listFromAndTo a b
  | a <= b = [a..b]
  | a > b = [a,a-1..b]

genCoordLst :: Coord -> Coord -> [Coord]
genCoordLst (ax,ay) (bx,by) = [(x,y) | x <- (listFromAndTo ax bx), y <- (listFromAndTo ay by)]

genSet :: Coord -> Coord -> Set.Set Coord
genSet a@(ax,ay) b@(bx,by) = Set.fromList (genCoordLst a b)

genSetCoords :: [Coord] -> Set.Set Coord
genSetCoords (a:b:xs) = Set.union (genSet a b) (genSetCoords (b:xs))
genSetCoords _ = Set.empty

extract (WireCons xs) = xs

genSetWire :: Wire -> Set.Set Coord
genSetWire (WireCons (a:b:xs)) = Set.union (genSet a b) (genSetWire $ WireCons (b:xs))
genSetWire (WireCons _) =  Set.empty

intersections :: Wire -> Wire -> [Coord]
intersections (WireCons w1) (WireCons w2) = Set.toList $ Set.intersection (genSetCoords $ tail w1) (genSetCoords $ tail w2)

followPath :: [[Coord]] -> Coord -> Int
followPath (line:lines) dest  = val
  where followedPath = followPath'' line dest 0
        val | isLeft followedPath = extractEither followedPath
            | otherwise = extractEither followedPath + (followPath lines dest) - 1
followPath _ _ = 0

followPath'' :: [Coord] -> Coord -> Int -> Either Int Int
followPath'' [] _ acc = Right acc
followPath'' (x:xs) dest acc
  | x == dest = Left acc
  | otherwise = followPath'' xs dest (acc+1)

numFromMaybe :: Num a => Maybe a -> a
numFromMaybe (Just a) = a
numFromMaybe Nothing = 0

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (Nothing:xs) = removeNothing xs
removeNothing ((Just a):xs) = a : removeNothing xs
