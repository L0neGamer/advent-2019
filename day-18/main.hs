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

type KeysToDoors = M.Map Point Point
type Accessible = ([(Point,Integer)],S.Set Point)

main = do
--        contents <- Useful.readFile "input.txt"
--        contents <- Useful.readFile "inputtest.txt"
--        contents <- Useful.readFile "inputtest2.txt"
        contents <- Useful.readFile "inputtest3.txt"
        let tmInit = parseOutput contents
            as = (allSpecials tm)
            end = as M.! (Door 'W')
            robot = head $ M.keys $ M.filter (\(tt,_) -> tt == Robot) tmInit
            tm = M.insert robot (Path, robot) tmInit
--            route = dijkstra robot tm end
            ktd = keysToDoors tm
--            (ktd',tm',acc') = freeUpDoor (17,29) tm ktd ([],S.empty)
            accessible = (getAccessibleKeys robot tm ktd S.empty)
            route = findBestRoute robot 0 tm ktd accessible
        putStr $ drawTileMap tm
        print $ ktd
--        print $ M.keys as
--        print route
--        print $ fst $ getAccessibleKeys robot tm ktd S.empty
--        print $ fst $ getAccessibleKeys robot tm' ktd' (snd acc')
        print $ allSpecials tm
        print accessible
        print $ route
        print $ map (\(p,_) -> fst (tm M.! p)) route
        putStr ""

findBestRoute' :: Point -> Integer -> TileMap -> KeysToDoors -> Accessible -> [(Point, Integer)]
findBestRoute' !p !dist !tm !ktd !acc = findBestRoute p dist tm' ktd acc'
  where (_, tm', acc') = freeUpDoor p tm ktd acc

sumDist :: [(Point, Integer)] -> Integer
sumDist xs = sum $ map snd xs

findBestRoute :: Point -> Integer -> TileMap -> KeysToDoors -> Accessible -> [(Point, Integer)]
findBestRoute curr dist tm ktd ([],_) = [(curr, dist)]
findBestRoute curr dist tm ktd accessible = (curr, dist) : getMinimumBy
  where nextRoutes = map (\(p,dist') -> findBestRoute' p dist' tm ktd accessible) (fst accessible)
        getMinimumBy = minimumBy (\a b -> sumDist a `compare` sumDist b) nextRoutes

--dijkstra :: Point -> TileMap -> Point -> Maybe ([Point], S.Set Point)
--dijkstra start tm end = dijkstra' start tm end S.empty

canAccess :: Point -> Point -> TileMap -> Bool
canAccess p p' tm = isJust $ fst $ dijkstra' p tm p' S.empty

dijkstra' :: Point -> TileMap -> Point -> S.Set Point -> (Maybe [Point], S.Set Point)
dijkstra' curr tm dest visited
  | curr == dest = (Just [dest],visited)
  | length notVisited == 0 = defaultRet
  | length routesOnNotVisited == 0 = defaultRet
  | otherwise = (Just (curr : shortestRoute), visitedNodes)
  where neighbours = getNeighbours curr
        notVisited = (S.fromList $ filter (\p -> p == dest || Path == fst (M.findWithDefault (Wall,p) p tm)) neighbours) S.\\ visited
        routes = S.map (\p -> dijkstra' p tm dest (S.insert curr visited)) notVisited
        routesOnNotVisited = S.toList $ S.map (fromJust.fst) $ S.filter (isJust.fst) routes
        visitedNodes = if length routes > 0 then foldl S.union visited (S.map snd routes) else visited
        shortestRoute = minimumBy (\a b -> length a `compare` length b) routesOnNotVisited
        defaultRet = (Nothing, visitedNodes)

getAccessibleKeys' :: (Maybe [a], b) -> (Maybe Integer,b)
getAccessibleKeys' ((Just xs), b) = (Just $ fromIntegral (length xs), b)
getAccessibleKeys' (Nothing, b) = (Nothing,b)

getKeys :: TileMap -> [Point]
getKeys tm = M.keys $ M.filter (isKey.fst) tm

getAccessibleKeys :: Point -> TileMap -> KeysToDoors -> S.Set Point -> Accessible
getAccessibleKeys p tm ktd s = (map (\(p,d,_) -> (p, fromJust d)) justJusts, setStuff)
  where toLocate = getKeys tm
        routeLengths = map (\k -> (k, getAccessibleKeys' (dijkstra' p tm k s))) toLocate
        justJusts = filter (\(k,d,_) -> k /= p && isJust d) $ map (\(k,(d,s')) -> (k,d,s')) routeLengths
        setStuff | length justJusts > 0 = foldl S.union s (map (\(_,_,s') -> s') justJusts)
                 | otherwise = s

freeUpDoor' :: Point -> (Maybe [a], S.Set Point) -> (Maybe (Point, Integer), S.Set Point)
freeUpDoor' p' (Just x, s') = (Just (p',fromIntegral $ length x),s')
freeUpDoor' p' x = (Nothing, snd x)

trace' x = trace (show x) x

freeUpDoor :: Point -> TileMap -> KeysToDoors -> Accessible -> (KeysToDoors, TileMap, Accessible)
freeUpDoor p tm ktd acc = (ktd, tm', acc')
  where door = if p `M.member` ktd then ktd M.! p else p
        removeKey = filter (\(p',_) -> p' /= p) (fst acc)
        tm' = M.insert door (Path, door) $ M.insert p (Path, p) tm
        newKeysDoor = if canAccess p door tm' then getAccessibleKeys door tm' ktd (S.empty) else ([],S.empty)
        newKeys' = getAccessibleKeys p tm' ktd (S.empty)
        newRoutes = map ((\p' -> freeUpDoor' p' $ dijkstra' p tm' p' S.empty).fst) (removeKey ++ fst newKeysDoor)
        updatedKeys = map (\(j,s) -> (fromJust j, s)) $ filter (isJust.fst) $ newRoutes
        acc' = ((map fst updatedKeys) ++ fst newKeys', S.union (snd acc) (foldl S.union (snd newKeys') (map snd updatedKeys)))

--nextMove :: Point -> TileMap -> KeysToDoors -> Integer


isIntersection :: Point -> TileMap -> Bool
isIntersection p tm = (Path == fst (M.findWithDefault (Wall,p) p tm)) && length neighbours' == 4
  where neighbours = getNeighbours p
        neighbours' = filter (\(t,_) -> t==Path) (map (\p' -> M.findWithDefault (Wall,p') p' tm) neighbours)

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

allSpecials :: TileMap -> M.Map TileType Point
allSpecials tm = M.fromList $ map (\(p,(tt,_)) -> (tt,p)) $ M.toList $ M.filter (\(tt,_) -> isSpecial tt) tm

keysToDoors :: TileMap -> KeysToDoors
keysToDoors tm = M.fromList $ map (\(tt,p) -> (keys M.! tt, p)) $ M.toList $ M.filter (\(x,y) -> x /= -1 && y /= -1) $ M.mapWithKey (\tt _ -> M.findWithDefault (-1,-1) (toDoor tt) doors) keys
  where ttp = allSpecials tm
        doors = M.filterWithKey (\tt _ -> isDoor tt) ttp
        keys = M.filterWithKey (\tt _ -> isKey tt) ttp

--parseOutputLine' :: [Char] -> Integer -> Integer -> TileMap
--parseOutputLine' [] _ _ = M.empty
--parseOutputLine' ('\n':xs) x y = parseOutput' xs 0 (y - 1)
--parseOutputLine' ('.':xs) x y = M.insert (x,y) (Path,(x,y)) (parseOutput' xs (x+1) y)
--parseOutputLine' ('#':xs) x y = M.insert (x,y) (Wall,(x,y)) (parseOutput' xs (x+1) y)
--parseOutputLine' ('@':xs) x y = M.insert (x,y) (Robot,(x,y)) (parseOutput' xs (x+1) y)
--parseOutputLine' ()
--parseOutputLine' xs x y = error $ show xs


subLists''' :: [a] -> Int -> [[a]]
subLists''' xs 0 = []
subLists''' xs i = (take i xs) : subLists''' xs (i - 1)

subLists'' :: [a] -> [[a]]
subLists'' [] = []
subLists'' lst@(x:xs) = subLists''' lst (length lst) ++ subLists'' xs
