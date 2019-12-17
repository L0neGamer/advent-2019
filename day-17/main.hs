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

a = [L,Fwd 12,R,Fwd 8,L,Fwd 6,R,Fwd 8,L,Fwd 6]
b = [R,Fwd 8,L,Fwd 12,L,Fwd 12,R,Fwd 8]
c = [L,Fwd 6,R,Fwd 6,L,Fwd 12]
mainStrs = "ABAABCBCCB"

main = do
        contents <- Useful.readFile "input.txt"
        let initMem = fromInput contents
            mem' = M.insert 0 2 initMem
            res = runProg (consPSFromMem initMem)
            input = [toInputBuffProg mainStrs, toInputBuffMvs a, toInputBuffMvs b, toInputBuffMvs c, [fromIntegral (ord 'n'),10]]
            ps = setInput (consPSFromMem mem') (concat input)
            res' = runProg ps
            out' = reverse $ out res
            roboMap = parseOutput $ out'
            intersections = M.keys (M.filterWithKey (\k _ -> (isIntersection k roboMap)) roboMap)
            roboMap' = M.mapWithKey (\k v -> if (k `elem` intersections) then (Vent,k) else v) roboMap
            route = getRoute roboMap
--            subLists = findSubLists route
--        print contents
--        print out'
--        putStr $ drawTileMap roboMap
--        print intersections
--        print $ map alignmentParam intersections
        print $ sum $ map alignmentParam intersections
--        putStr (drawTileMap roboMap')
--        print route
--        print input
--        putStr $ map (chr.fromIntegral) $ reverse $ out res'
        print $ head $ out res'
--        print subLists
--        print $ S.fromList $ fromJust subLists
        putStr ""

alignmentParam :: Point -> Integer
alignmentParam (x, y) = ((abs x)) * ((abs y))

isIntersection :: Point -> TileMap -> Bool
isIntersection p tm = (Path == fst (M.findWithDefault (Wall,p) p tm)) && length neighbours' == 4
  where neighbours = getNeighbours p
        neighbours' = filter (\(t,_) -> t==Path) (map (\p' -> M.findWithDefault (Wall,p') p' tm) neighbours)

parseOutput :: OutputVals -> TileMap
parseOutput o = parseOutput' o 0 0
--  where initMap =
--        (max_y, res) = M.mapAccumWithKey (\y (_,y') b -> (max y y',b)) 0 initMap
--        res' = M.mapKeys (\(x,y) -> (x,max_y-y)) res

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

findSubLists'' :: (Eq a) => [a] -> [a] -> Bool
findSubLists'' finding [] = False
findSubLists'' finding xs = (finding == take len xs) || findSubLists'' finding (drop len xs)
  where len = length finding

toIntFromMv :: Move -> [Integer]
toIntFromMv L = [76]
toIntFromMv R = [82]
toIntFromMv (Fwd x) = map (fromIntegral.ord) (show x)

toInputBuffMvs :: [Move] -> [Integer]
toInputBuffMvs mvs = (map (fromIntegral.ord) (tail $ init $ show mvs)) ++ [10]

toInputBuffProg :: String -> [Integer]
toInputBuffProg xs = (intersperse 44 (map (fromIntegral.ord) xs)) ++ [10]

--findSubLists' :: (Eq a, Show a, Ord a) => [a] -> Int -> Maybe S.Set [a]
--findSubLists' [] i = S.empty
--findSubLists' xs i
--  | currSub `isInfixOf` rest && isJust recurseNext = recurseNext
--  | not (currSub `isInfixOf` rest) && i == 1 = Nothing
--  | isJust finNext = S.insert currSub (fromJust finNext)
--  | otherwise = S.insert currSub ()
--  where currSub = take i xs
--        rest = drop i xs
--        recurseNext = findSubLists' xs (i + 1)
--        finNext = findSubLists rest
--findSubLists' xs i
--  | finding `isInfixOf` rest && length next < 4 = trace (show next) next
--  | otherwise = S.insert (take (i-1) xs) $ findSubLists' (drop (i-1) xs) 1
--  where finding = take i xs
--        rest = drop i xs
--        next = findSubLists' xs (i + 1)
--  | i > 1 && isJust next' = Just $ (take i xs : fromJust next')

--findSubLists xs = findSubLists' xs 1
