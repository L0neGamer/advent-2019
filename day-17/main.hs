{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
import Useful
import IntCode
import Data.String
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Char (ord,chr)
import qualified Data.Map as M
import qualified Data.Set as S

type Signal = [Integer]
type Phase = [Integer]

data Move = L | R | Fwd Integer deriving (Eq, Ord)

instance Show Move where
  show L = "L"
  show R = "R"
  show (Fwd i) = show i

main = do
        contents <- Useful.readFile "input.txt"
        let initMem = fromInput contents
        print $ getAlignments initMem
        print $ getDustCleaned initMem
        putStr ""

getMap :: Mem -> TileMap
getMap mem = parseOutput $ reverse $ out $ runProg (consPSFromMem mem)

getDustCleaned :: Mem -> Integer
getDustCleaned mem = head $ out $ runProg $ setInput (consPSFromMem (M.insert 0 2 mem)) (getBuffer robotMap)
  where robotMap = getMap mem

getAlignments :: Mem -> Integer
getAlignments mem = sum $ map alignmentParam (M.keys (M.filterWithKey (\k _ -> (isIntersection k roboMap)) roboMap))
  where roboMap = parseOutput $ reverse $ out (runProg (consPSFromMem mem))

alignmentParam :: Point -> Integer
alignmentParam (x, y) = ((abs x)) * ((abs y))

isIntersection :: Point -> TileMap -> Bool
isIntersection p tm = (Path == fst (M.findWithDefault (Wall,p) p tm)) && length neighbours' == 4
  where neighbours = getNeighbours p
        neighbours' = filter (\(t,_) -> t==Path) (map (\p' -> M.findWithDefault (Wall,p') p' tm) neighbours)

parseOutput :: OutputVals -> TileMap
parseOutput o = parseOutput' o 0 0

parseOutput' :: OutputVals -> Integer -> Integer -> TileMap
parseOutput' [] _ _ = M.empty
parseOutput' (10:xs) x y = parseOutput' xs 0 (y - 1)
parseOutput' (35:xs) x y = M.insert (x,y) (Path,(x,y)) (parseOutput' xs (x+1) y)
parseOutput' (46:xs) x y = M.insert (x,y) (Wall,(x,y)) (parseOutput' xs (x+1) y)
parseOutput' (94:xs) x y = M.insert (x,y) (Robot,(x,y)) (parseOutput' xs (x+1) y)
parseOutput' xs x y = error $ show xs

checkMove :: Move -> Point -> Bearing -> TileMap -> Maybe ([Move], (Point, Bearing))
checkMove (Fwd _) curr bearing tm
  | l > 0 = Just ([Fwd l], (p, bearing))
  | otherwise = Nothing
  where (l, p) = findLine curr bearing tm
checkMove mv curr bearing tm
  | isJust canGo = Just (mv:xs, p)
  | otherwise = Nothing
  where rotation L = bearingAntiClockwise
        rotation R = bearingClockwise
        bearing' = (rotation mv) bearing
        canGo = checkMove (Fwd 1) curr bearing' tm
        (xs, p) = fromJust canGo

findLine :: Point -> Bearing -> TileMap -> (Integer, Point)
findLine p b tm
  | fst (M.findWithDefault (Wall,p') p' tm) == Path = (l+1, p'')
  | otherwise = (0, p)
  where p' = moveInBearing p b
        (l, p'') = findLine p' b tm

getRoute' :: Point -> Bearing -> TileMap -> [Move]
getRoute' curr bearing tm
  | isJust fwd = unpack fwd
  | isJust l   = unpack l
  | isJust r   = unpack r
  | otherwise  = []
  where fwd = checkMove (Fwd 1) curr bearing tm
        l = checkMove L curr bearing tm
        r = checkMove R curr bearing tm
        unpack (Just (mvs, (p,b))) = mvs ++ getRoute' p b tm

getRoute :: TileMap -> [Move]
getRoute tm = getRoute' start N tm
  where (start,_) = M.mapAccumWithKey (\a key v@(tile,_) -> if tile==Robot then (key,v) else (a,v)) (-1,-1) tm

toIntFromMv :: Move -> [Integer]
toIntFromMv L = [76]
toIntFromMv R = [82]
toIntFromMv (Fwd x) = map (fromIntegral.ord) (show x)

toInputBuffMvs :: [Move] -> [Integer]
toInputBuffMvs mvs = (map (fromIntegral.ord) (tail $ init $ show mvs)) ++ [10]

toInputBuffProg :: String -> [Integer]
toInputBuffProg xs = (intersperse 44 (map (fromIntegral.ord) xs)) ++ [10]

matchBeginningOf :: (Eq a) => [a] -> [a] -> Bool
matchBeginningOf xs ys = length xs <= length ys && xs == (take (length xs) ys)

occursIn :: (Eq a) => [a] -> [a] -> Integer
occursIn _ [] = 0
occursIn xs ys
  | xs `matchBeginningOf` ys = 1 + rest
  | otherwise = rest
  where rest = occursIn xs (tail ys)

subLists''' :: [a] -> Int -> [[a]]
subLists''' xs 0 = []
subLists''' xs i = (take i xs) : subLists''' xs (i - 1)

subLists'' :: [a] -> [[a]]
subLists'' [] = []
subLists'' lst@(x:xs) = subLists''' lst (length lst) ++ subLists'' xs

isTurn L = True
isTurn R = True
isTurn (Fwd _) = False

subLists' :: [Move] -> S.Set [Move]
subLists' as = as'
  where as' = S.filter (\xs -> length xs > 2 && length xs < 20 && occursIn xs as > 1 && isTurn (head xs) && (not $ isTurn (last xs))) $ S.fromList $ subLists'' as

--subLists :: [Move] -> [([Move], [Move], [Move])]
subLists xs = ret
  where candidates = S.toList $ subLists' xs
        listLen = length candidates - 1
        ret = [(candidates!!x,candidates!!y,candidates!!z) | x <- [0..listLen], y <- [x..listLen], z <- [y..listLen], x /= y && y /= z]
--        candidPerms = map (\(x:y:[z]) -> (x,y,z)) $ filter (\xs -> length xs == 3) $ sortBy (\a b -> length a `compare` length b) $ subsequences (S.toList candidates)

testInp :: [Move] -> ([Move],[Move],[Move]) -> Maybe ([String],([Move],[Move],[Move]))
testInp [] tup@(a, b, c) = Just ([],tup)
testInp xs tup@(a, b, c)
  | (a `matchBeginningOf` xs) && isJust restA = Just $ ("A": (fst.fromJust) restA, tup)
  | (b `matchBeginningOf` xs) && isJust restB = Just $ ("B": (fst.fromJust) restB, tup)
  | (c `matchBeginningOf` xs) && isJust restC = Just $ ("C": (fst.fromJust) restC, tup)
  | otherwise = Nothing
  where restA = testInp (drop (length a) xs) tup
        restB = testInp (drop (length b) xs) tup
        restC = testInp (drop (length c) xs) tup

getBuffer :: TileMap -> InputVals
getBuffer roboMap = concat [toInputBuffProg (concat funcCalls), toInputBuffMvs a, toInputBuffMvs b, toInputBuffMvs c, [fromIntegral (ord 'n'),10]]
  where route = getRoute roboMap
        moveCombos = subLists route
        validCombos = map fromJust $ filter (isJust) $ map (testInp route) moveCombos
        combo@(funcCalls,(a,b,c)) = head validCombos
