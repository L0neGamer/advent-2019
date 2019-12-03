import System.IO
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read

type Coord = (Int, Int)
data Dir = R | D | L | U deriving (Show, Eq)
type Direction = (Dir, Int)
--data Wire = WireCons Coord Wire | WireEnd deriving (Show, Eq)
data Wire = WireCons [Coord] deriving (Show, Eq)

--instance Foldable Wire where
--  foldr f z (WireCons xs) = WireCons

main = do
        contents <- readFile "input.txt"
        print "arg"
--        let testMap = fromStr contents
--            val' = execute 0 (Just (Map.insert 2 21 (Map.insert 1 76 testMap)))
--            val = iter (map (\a -> (execute 0 (Just a))) (allPerms testMap)) 0 0
--        print val
        print "val"

--iter :: [Maybe (Map Int Int)] -> Int -> Int -> Maybe (Int, Int)
--iter [] noun verb = Nothing
--iter ((Just x):xs) noun verb
--  | booleanVal =  Just (verb, noun) -- these are named incorrectly
--  | otherwise = iter xs newNoun newVerb
--  where initVal = Map.lookup 0 x
--        booleanVal = case (initVal) of
--                        (Just 19690720) -> True
--                        (Just x) -> False
--                        Nothing -> False
--        newNoun = mod (noun + 1) 100
--        newVerb | newNoun < noun = verb + 1
--                | otherwise = verb



--allPerms :: Map Int Int -> [Map Int Int]
--allPerms myMap = allPerms' (allPerms' [myMap] 1 99) 2 99
--allPerms' :: [Map Int Int] -> Int -> Int -> [Map Int Int]
--allPerms' [] _ _ = []
--allPerms' (x:xs) index value = (map (\a -> Map.insert index a x) [0..value]) ++ (allPerms' xs index value)

splitStr :: String -> Char -> [String]
splitStr str split = wordsWhen (==split) str

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

maybeIfy :: [Maybe a] -> Maybe [a]
maybeIfy [] = Just []
maybeIfy (Nothing:xs) = Nothing
maybeIfy ((Just a):xs) = maybeIfy xs >>= \x -> Just (a:x)

toWire :: [Direction] -> Int -> Int -> Wire
toWire [] _ _ = WireCons []
toWire ((R, i):xs) x y = WireCons (x+i, y) (toWire xs (x+i) y)

--toWire dirs x y = WireCons $ foldl drawLine [(x, y)] dirs

toWire ((L, i):xs) x y = WireCons (x-i, y) (toWire xs (x-i) y)
toWire ((U, i):xs) x y = WireCons (x, y+i) (toWire xs x (y+i))
toWire ((D, i):xs) x y = WireCons (x, y-i) (toWire xs x (y-i))

--drawLine :: Direction -> [Coord] -> [Coord]
--drawLine dir coords = coords ++ (executeDir dir $ last coords)
--
--executeDir :: Direction -> Coord -> Coord
--executeDir (R, i) (x, y) = (x + i, y)

fromInputToWires :: String -> [Maybe Wire]
fromInputToWires str = wires
  where lines = splitStr str '\n'
        dirs = map (\a -> maybeIfy (fromStrLstToDirections (splitStr a ','))) lines
        wires = map (\dir -> dir >>= \undir -> Just (toWire undir 0 0)) dirs

findIntersection :: Coord -> Coord -> Coord -> Coord -> Maybe Coord
findIntersection (x1,y1) (x2,y2) (x3,y3) (x4,y4)
  | t >= 0 && t <= 1 = Just (round ((fromIntegral x1)+t*(fromIntegral (x2-x1))), round ((fromIntegral y1)+t*(fromIntegral (y2-y1))))
  | otherwise = Nothing
  where denom = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
        t | denom == 0 = -1 :: Float
          | otherwise = (fromIntegral ((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4))) / (fromIntegral denom) :: Float
--  where t = div ((x1-x2)(y2-1) - (y1-y2)(x2-1)) ((x1-1)(y2-1) - (y1-1)(x2-1))

intersections :: Wire -> Wire -> [Maybe Coord]
intersections (WireEnd) _ = []
intersections _ (WireEnd) = []
intersections (WireCons _ WireEnd) _ = []
intersections _ (WireCons _ WireEnd) = []
intersections (WireCons coord1 (WireCons coord1' wire1)) (WireCons coord2 (WireCons coord2' wire2)) = findIntersection coord1 coord1' coord2 coord2' : intersections (WireCons coord1' wire1) (WireCons coord2' wire2) ++ intersections (WireCons coord1 (WireCons coord1' wire1)) (WireCons coord2' wire2) ++ intersections (WireCons coord1' wire1) (WireCons coord2 (WireCons coord2' wire2))

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (Nothing:xs) = removeNothing xs
removeNothing ((Just a):xs) = a : removeNothing xs

removeDupes xs = removeDupes' xs []

removeDupes' :: Eq a => [a] -> [a] -> [a]
removeDupes' [] xs = xs
removeDupes' (a:as) xs
  | a `elem` xs = removeDupes' as xs
  | otherwise = removeDupes' as (a:xs)

findAllIntersections :: [Wire] -> [Coord]
findAllIntersections [] = []
findAllIntersections xs = removeDupes $ removeNothing $ intersections (xs!!0) (xs!!1) --concat $ map (\(w1, w2) -> intersections w1 w2) [(w1, w2)| w1 <- xs, w2 <- xs, not $w1 == w2]

--toWire :: [Direction] -> Wire
--
--helper :: [a] -> [(Int, a)]
--helper a = helper' a 0
--
--helper' :: [a] -> Int -> [(Int, a)]
--helper' [] _ = []
--helper' (a:as) b = (b, a):  helper' as (b+1)
--
---- thanks to https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
--
--quickOp :: (Int -> Int -> Int) -> Int -> Map Int Int -> Maybe (Map Int Int)
--quickOp f instr myMap =  execute (instr+4) $ res
--  where quickLookup a = Map.lookup a myMap
--        res = do
--                a <- quickLookup (instr+1) >>= quickLookup
--                b <- quickLookup (instr+2) >>= quickLookup
--                c <- quickLookup (instr+3)
--                return $ Map.insert (c) (f a b) myMap
--
--execute :: Int -> Maybe (Map Int Int) -> Maybe (Map Int Int)
--execute instr Nothing = Nothing
--execute instr (Just myMap) = res
--  where quickLookup a = Map.lookup a myMap
--        res = do
--                instrOp <- quickLookup instr
--                quickOp' instrOp instr myMap
--
--quickOp' :: Int -> Int -> Map Int Int -> Maybe (Map Int Int)
--quickOp' 1 a b = quickOp (+) a b
--quickOp' 2 a b = quickOp (*) a b
--quickOp' 99 _ myMap = Just myMap
--quickOp' _ _ _ = Nothing
--
--execute' :: String -> Maybe (Map Int Int)
--execute' str = execute 0 (Just $ fromStr str)
