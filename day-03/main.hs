import System.IO
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Read
import Data.List

type Coord = (Int, Int)
data Dir = R | D | L | U deriving (Show, Eq)
type Direction = (Dir, Int)
--data Wire = WireCons Coord Wire | WireEnd deriving (Show, Eq)
data Wire = WireCons [Coord] deriving (Show, Eq)

--instance Foldable Wire where
--  foldr f z (WireCons xs) = WireCons

contents = "R8,U5,L5,D3\nU7,R6,D4,L4"
--contents = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
--contents = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
wires = removeNothing $ fromInputToWires contents
inters = intersections (wires!!0) (wires!!1)
route1 = extract (wires!!0)
route2 = extract (wires!!1)
leftPath = followPath route1 (3,3)
rightPath = followPath route2 (3,3)
f dest = (followPath route1 dest) + (followPath route2 dest)
res = map f inters

main = do
        contents <- readFile "input.txt"
        print "arg"
        let
--            contents = "R8,U5,L5,D3\nU7,R6,D4,L4"
            wires = removeNothing $ fromInputToWires contents
            inters = intersections (wires!!0) (wires!!1)
            route1 = extract (wires!!0)
            route2 = extract (wires!!1)
            leftPath = followPath route1 (6,5)
            rightPath = followPath route2 (6,5)
            f dest = (followPath route1 dest) + (followPath route2 dest)
            res = map f inters

--        let testMap = fromStr contents
--            val' = execute 0 (Just (Map.insert 2 21 (Map.insert 1 76 testMap)))
--            val = iter (map (\a -> (execute 0 (Just a))) (allPerms testMap)) 0 0
--        print val
        print $ sol contents
        print $ inters
        print leftPath
        print rightPath
        print $ minimum res


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

listFromAndTo a b
  | a <= b = [a..b]
  | a > b = reverse [b..a]

genCoordLst :: Coord -> Coord -> [Coord]
genCoordLst a@(ax,ay) b@(bx,by) = [(x,y) | x <- (listFromAndTo ax bx), y <- (listFromAndTo ay by)]
--  where xbot = min ax bx
--        xtop = max ax bx
--        ybot = min ay by
--        ytop = max ay by

genSet :: Coord -> Coord -> Set.Set Coord
genSet a@(ax,ay) b@(bx,by) = Set.fromList (genCoordLst a b)

genSetCoords :: [Coord] -> Set.Set Coord
genSetCoords coords@(a:b:xs) = Set.union (genSet a b) (genSetCoords (b:xs))
genSetCoords _ = Set.empty

extract (WireCons xs) = xs

genSetWire :: Wire -> Set.Set Coord
genSetWire (WireCons (a:b:xs)) = Set.union (genSet a b) (genSetWire $ WireCons (b:xs))
genSetWire (WireCons _) =  Set.empty

intersections :: Wire -> Wire -> [Coord]
intersections (WireCons w1) (WireCons w2) = Set.toList $ Set.intersection (genSetCoords $ tail w1) (genSetCoords $ tail w2)

extractEither :: Either a a -> a
extractEither (Left a) = a
extractEither (Right a) = a

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

followPath :: [Coord] -> Coord -> Int
followPath (a:b:xs) dest = val
  where followedPath = followPath' a b dest
        val | isLeft followedPath = extractEither followedPath
            | otherwise = extractEither followedPath + followPath (b:xs) dest - 1
followPath _ _ = 0

followPath' :: Coord -> Coord -> Coord -> Either Int Int
followPath' c1 c2 dest = ret
  where lineLst = genCoordLst c1 c2
        ret = followPath'' (lineLst) dest 0

followPath'' :: [Coord] -> Coord -> Int -> Either Int Int
followPath'' [] _ acc = Right (acc)
followPath'' (x:xs) dest acc
  | x == dest = Left (acc)
  | otherwise = followPath'' xs dest (acc+1)

numFromMaybe :: Num a => Maybe a -> a
numFromMaybe (Just a) = a
numFromMaybe Nothing = 0

--intersections (WireCons (x1:x2:xs)) (WireCons (y1:y2:ys)) = findIntersection x1 x2 y1 y2 : intersections (WireCons (x2:xs)) (WireCons (y2:ys)) ++ intersections (WireCons (x1:x2:xs)) (WireCons (y2:ys)) ++ intersections (WireCons (x2:xs)) (WireCons (y1:y2:ys))
--intersections _ _ = []
--intersections (WireEnd) _ = []
--intersections _ (WireEnd) = []
--intersections (WireCons _ WireEnd) _ = []
--intersections _ (WireCons _ WireEnd) = []
--intersections (WireCons coord1 (WireCons coord1' wire1)) (WireCons coord2 (WireCons coord2' wire2)) = findIntersection coord1 coord1' coord2 coord2' : intersections (WireCons coord1' wire1) (WireCons coord2' wire2) ++ intersections (WireCons coord1 (WireCons coord1' wire1)) (WireCons coord2' wire2) ++ intersections (WireCons coord1' wire1) (WireCons coord2 (WireCons coord2' wire2))

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
findAllIntersections xs = removeDupes $ intersections (xs!!0) (xs!!1) --concat $ map (\(w1, w2) -> intersections w1 w2) [(w1, w2)| w1 <- xs, w2 <- xs, not $w1 == w2]

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
