{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
import Useful
import IntCode
import Data.String
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Char (ord,chr,isLower,isUpper,toUpper)
import qualified Data.Map as M
import qualified Data.Set as S

type Keys = M.Map Point TileType
type Doors = M.Map Point TileType
type KeyToKey = M.Map Point (M.Map Point (Integer, [Point]))

data NavState = NavStat [Point] Point Doors Keys Integer

main = do
--        contents <- Useful.readFile "input.txt"
        contents <- Useful.readFile "inputtest.txt"
--        contents <- Useful.readFile "inputtest2.txt"
--        contents <- Useful.readFile "inputtest3.txt"
        let tmInit = parseOutput contents
            (tm, keys, doors, robot) = getDetails tmInit
        putStr $ drawTileMap tm
        putStr $ drawTileMap tmInit
        print $ (keys, doors, robot)
        print $ aStar tm (15,3) (1,1)
        print $ getKeyToKey tm keys doors
        putStr ""

pt1' :: [NavState] -> TileMap -> KeyToKey -> Keys -> Doors -> NavState
pt1' [] tm ktk k d =
pt1' navStats tm ktk k d

pt1 :: String -> NavState
pt1 str = pt1' [(NavStat [] robot (M.keys doors) (M.keys keys1) 0)] tm keyToKey keys doors
  where tm' = parseOutput str
        (tm, keys, doors, robot) = getDetails tm'
        keyToKey = getKeyToKey tm doors keys

-- https://pastebin.com/75wQ5unK
getKeyToKey' :: TileMap -> Doors -> Point -> Point -> (Integer, [Point])
getKeyToKey' tm doors key key' = (fromIntegral (length path) - 2, reverse doors')
  where path = fromJust $ aStar tm key key'
        doors' = filter (\p -> p `M.member` doors) path

getKeyToKey :: TileMap -> Keys -> Doors -> KeyToKey
getKeyToKey tm keys doors = M.mapWithKey (\k _ -> M.delete k $ M.mapWithKey (\k' _ -> getKeyToKey' tm doors k k') keys) keys

simplifyTT :: TileType -> TileType
simplifyTT Wall = Wall
simplifyTT _ = Path

getDetails :: TileMap -> (TileMap, Keys, Doors, Point)
getDetails tm = (clearedTM, keys, doors, robot)
  where clearedTM = M.map (\(tt,p) -> (simplifyTT tt, p)) tm
        keys = M.map (\(tt,_) -> tt) $ M.filter (\(tt,_) -> isKey tt) tm
        doors = M.map (\(tt,_) -> tt) $ M.filter (\(tt,_) -> isDoor tt) tm
        robot = head $ M.keys $ M.filter (\(tt,_) -> tt == Robot) tm

parseOutput :: String -> TileMap
parseOutput o = foldl (M.union) M.empty (map (\(y,line) -> parseLine line 0 y) lines')
  where lines = reverse $ splitOn "\n" o
        lines' = zip [0..] lines

parseLine :: [Char] -> Integer -> Integer -> TileMap
parseLine [] _ _ = M.empty
parseLine ('\n':xs) x y = M.empty
parseLine ('.':xs) x y = M.insert (x,y) (Path,(x,y)) (parseLine xs (x+1) y)
parseLine ('#':xs) x y = M.insert (x,y) (Wall,(x,y)) (parseLine xs (x+1) y)
parseLine ('@':xs) x y = M.insert (x,y) (Robot,(x,y)) (parseLine xs (x+1) y)
parseLine (c:xs) x y = M.insert (x,y) (designation,(x,y)) (parseLine xs (x+1) y)
  where designation | isUpper c = Door c
                    | isLower c = Key c
                    | otherwise = error ("invalid character:" ++ [c])

aStar :: TileMap -> Point -> Point -> Maybe [Point]
aStar tm start dest = aStar' tm dest (S.singleton start) fromStart totalPath heuristic
  where heuristic = manhattan dest
        fromStart = M.fromList [(start,0)]
        totalPath = M.fromList [(start, heuristic start)]

aStar' :: TileMap -> Point -> S.Set Point -> M.Map Point Integer -> M.Map Point Integer -> (Point -> Integer) -> Maybe [Point]
aStar' tm dest open fromStart totalPath h
  | S.size open == 0 = Nothing
  | o == dest = Just $ reconstruct tm dest
  | otherwise = aStar' tm' dest open' fromStart' totalPath' h
  where o = minimumBy (\p p'-> (totalPath M.! p) `compare` (totalPath M.! p)) (S.toList open)
        neighbours = filter (\p -> Path == (fst $ M.findWithDefault (Wall,p) p tm)) $ getNeighbours o
        neighbours' = filter (\p -> fromStart M.! o + 1 < M.findWithDefault (2 * (1 + fromStart M.! o)) p fromStart) neighbours
        tm' = M.union (M.fromList (map (\p -> (p,(Path,o))) neighbours')) tm
        fromStart' = M.union (M.fromList (map (\p -> (p,fromStart M.! o + 1))neighbours')) fromStart
        totalPath' = M.union (M.fromList (map (\p -> (p,fromStart' M.! p + h p)) neighbours')) fromStart
        open' = S.union (S.fromList neighbours') $ S.delete o open

reconstruct :: TileMap -> Point -> [Point]
reconstruct tm p
  | p == pred = [p]
  | otherwise = p:reconstruct tm pred
  where (_,pred) = tm M.! p

dijkstra :: TileMap -> Point -> Point -> Maybe [Point]
dijkstra tm start end = dijkstra' tm start end S.empty

--canAccess :: Point -> Point -> TileMap -> Bool
--canAccess p p' tm = isJust $ fst $ dijkstra' p tm p' S.empty

dijkstra' :: TileMap -> Point -> Point -> S.Set Point -> Maybe [Point]
dijkstra' tm curr dest visited
  | curr == dest = Just [dest]
  | length notVisited == 0 = Nothing
  | length routesOnNotVisited == 0 = Nothing
  | otherwise = Just $ curr : shortestRoute
  where neighbours = getNeighbours curr
        notVisited = sortBy (\a b -> manhattan a dest `compare` manhattan b dest) $ S.toList $ (S.fromList $ filter (\p -> p == dest || Path == fst (M.findWithDefault (Wall,p) p tm)) neighbours) S.\\ visited
        routes = map (\p -> dijkstra' tm p dest (S.insert curr visited)) notVisited
        routesOnNotVisited = map fromJust $ filter isJust routes
        shortestRoute = minimumBy (\a b -> length a `compare` length b) routesOnNotVisited

--getAccessibleKeys' :: (Maybe [a], b) -> (Maybe Integer,b)
--getAccessibleKeys' ((Just xs), b) = (Just $ fromIntegral (length xs), b)
--getAccessibleKeys' (Nothing, b) = (Nothing,b)
--
--getKeys :: TileMap -> [Point]
--getKeys tm = M.keys $ M.filter (isKey.fst) tm
--
--getAccessibleKeys :: Point -> TileMap -> KeysToDoors -> S.Set Point -> Accessible
--getAccessibleKeys p tm ktd s = (map (\(p,d,_) -> (p, fromJust d)) justJusts, setStuff)
--  where toLocate = getKeys tm
--        routeLengths = map (\k -> (k, getAccessibleKeys' (dijkstra' p tm k s))) toLocate
--        justJusts = filter (\(k,d,_) -> k /= p && isJust d) $ map (\(k,(d,s')) -> (k,d,s')) routeLengths
--        setStuff | length justJusts > 0 = foldl S.union s (map (\(_,_,s') -> s') justJusts)
--                 | otherwise = s
--
--freeUpDoor' :: Point -> (Maybe [a], S.Set Point) -> (Maybe (Point, Integer), S.Set Point)
--freeUpDoor' p' (Just x, s') = (Just (p',fromIntegral $ length x),s')
--freeUpDoor' p' x = (Nothing, snd x)
--
--trace' x = trace (show x) x
--
--freeUpDoor :: Point -> TileMap -> KeysToDoors -> Accessible -> (KeysToDoors, TileMap, Accessible)
--freeUpDoor p tm ktd acc = (ktd, tm', acc')
--  where door = if p `M.member` ktd then ktd M.! p else p
--        removeKey = filter (\(p',_) -> p' /= p) (fst acc)
--        tm' = M.insert door (Path, door) $ M.insert p (Path, p) tm
--        newKeysDoor = if canAccess p door tm' then getAccessibleKeys door tm' ktd (S.empty) else ([],S.empty)
--        newKeys' = getAccessibleKeys p tm' ktd (S.empty)
--        newRoutes = map ((\p' -> freeUpDoor' p' $ dijkstra' p tm' p' S.empty).fst) (removeKey ++ fst newKeysDoor)
--        updatedKeys = map (\(j,s) -> (fromJust j, s)) $ filter (isJust.fst) $ newRoutes
--        acc' = ((map fst updatedKeys) ++ fst newKeys', S.union (snd acc) (foldl S.union (snd newKeys') (map snd updatedKeys)))
--
----nextMove :: Point -> TileMap -> KeysToDoors -> Integer
--
--
--isIntersection :: Point -> TileMap -> Bool
--isIntersection p tm = (Path == fst (M.findWithDefault (Wall,p) p tm)) && length neighbours' == 4
--  where neighbours = getNeighbours p
--        neighbours' = filter (\(t,_) -> t==Path) (map (\p' -> M.findWithDefault (Wall,p') p' tm) neighbours)
--
--
--allSpecials :: TileMap -> M.Map TileType Point
--allSpecials tm = M.fromList $ map (\(p,(tt,_)) -> (tt,p)) $ M.toList $ M.filter (\(tt,_) -> isSpecial tt) tm
--
--keysToDoors :: TileMap -> KeysToDoors
--keysToDoors tm = M.fromList $ map (\(tt,p) -> (keys M.! tt, p)) $ M.toList $ M.filter (\(x,y) -> x /= -1 && y /= -1) $ M.mapWithKey (\tt _ -> M.findWithDefault (-1,-1) (toDoor tt) doors) keys
--  where ttp = allSpecials tm
--        doors = M.filterWithKey (\tt _ -> isDoor tt) ttp
--        keys = M.filterWithKey (\tt _ -> isKey tt) ttp
--
----parseOutputLine' :: [Char] -> Integer -> Integer -> TileMap
----parseOutputLine' [] _ _ = M.empty
----parseOutputLine' ('\n':xs) x y = parseOutput' xs 0 (y - 1)
----parseOutputLine' ('.':xs) x y = M.insert (x,y) (Path,(x,y)) (parseOutput' xs (x+1) y)
----parseOutputLine' ('#':xs) x y = M.insert (x,y) (Wall,(x,y)) (parseOutput' xs (x+1) y)
----parseOutputLine' ('@':xs) x y = M.insert (x,y) (Robot,(x,y)) (parseOutput' xs (x+1) y)
----parseOutputLine' ()
----parseOutputLine' xs x y = error $ show xs
--
--
--subLists''' :: [a] -> Int -> [[a]]
--subLists''' xs 0 = []
--subLists''' xs i = (take i xs) : subLists''' xs (i - 1)
--
--subLists'' :: [a] -> [[a]]
--subLists'' [] = []
--subLists'' lst@(x:xs) = subLists''' lst (length lst) ++ subLists'' xs

isSpecial :: TileType -> Bool
isSpecial (Door _) = True
isSpecial (Key _) = True
isSpecial _ = False

isDoor :: TileType -> Bool
isDoor (Door _) = True
isDoor _ = False
isKey :: TileType -> Bool
isKey (Key _) = True
isKey _ = False
toDoor :: TileType -> TileType
toDoor (Key c) = Door (toUpper c)