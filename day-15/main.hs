{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
import Useful
import IntCode
import Data.String
import Data.List
import Debug.Trace
import Data.Maybe
import qualified Data.Map as M

main = do
        contents <- Useful.readFile "input.txt"
        let (tm, end) = navigationHarness' (runStr contents [])
            path = dirtyGetPath end tm
            oxygen = fillWithOxygen 0 [end] tm
--        print $ end
--        print $ tm M.! end
--        putStr $ drawTileMap tm
--        print $ path
        print $ length path
        print $ oxygen
        putStr ""

--pathBetween' :: Point -> Point -> [Point] -> TileMap -> [Point]
--pathBetween' curr dest pred tm
--  | curr == dest = []
--  |
--  where surrounding = filter ()

fillWithOxygen'' :: Point -> TileMap -> [Point]
fillWithOxygen'' curr tm = surrounding
  where surrounding = filter (\p -> 1 == fst (M.findWithDefault (0,(0,0)) p tm)) $ map (moveInBearing curr) [N,S,E,W]

fillWithOxygen' :: [Point] -> TileMap -> TileMap
fillWithOxygen' [] tm = tm
fillWithOxygen' (x:xs) tm = M.insert x (3, x) (fillWithOxygen' xs tm)

fillWithOxygen :: Integer -> [Point] -> TileMap -> Integer
fillWithOxygen acc [] _ = acc - 1
fillWithOxygen acc xs tm = fillWithOxygen (acc + 1) xs' tm'
  where tm' = fillWithOxygen' xs tm
        xs' = concat $ map (flip fillWithOxygen'' tm) xs

updateMap :: TileType -> Point -> Point -> TileMap -> TileMap
updateMap 0 curr newPos tm = M.insertWith (\_ b -> b) newPos (0,curr) tm
updateMap 1 curr newPos tm = M.insertWith (\_ b -> b) newPos (1,curr) tm
updateMap 2 curr newPos tm = M.insertWith (\_ b -> b) newPos (2,curr) tm

chooseDir :: Point -> Point -> TileMap -> Bearing
chooseDir curr parent tm = res
  where allDirs = (map (\b -> ((\p -> M.findWithDefault (-1, (0,0)) p tm) (moveInBearing curr b), b)) [N, W, E, S]) :: [(Tile,Bearing)]
        unexplored = filter (\((x,_),_) -> x==(-1)) allDirs
        res | length unexplored > 0 = snd $ head unexplored
            | otherwise = getBearing curr parent

findTileType :: TileType -> TileMap -> Maybe Point
findTileType tt tm
  | length tm' > 0 = Just $ fst $ head tm'
  | otherwise = Nothing
  where tm' = filter (\(_,(tt',_)) -> tt' == tt) $ M.toList tm

navigationHarness :: Point -> Bearing -> TileMap -> ProgramState -> (TileMap, Point)
navigationHarness curr bearing tm ps = res
  where ps'@ProgStat{..} = runProg (flip setEndState Running $ flip setInput [convertB bearing] $ clearBuffs ps)
        newPos = moveInBearing curr bearing
        tileType =  head out
        tm' = updateMap tileType curr newPos tm
        curr' | tileType > 0 = newPos
              | otherwise = curr
        newDir = chooseDir curr' (snd $ tm' M.! curr') tm
        oxyVentLoc = findTileType 2 tm'
        res | curr' == (0, 0) && isJust oxyVentLoc = (tm', fromJust oxyVentLoc)
            | otherwise = navigationHarness curr' newDir tm' ps'

navigationHarness' :: ProgramState -> (TileMap, Point)
navigationHarness' ps = navigationHarness (0,0) N (M.empty) ps

convertB :: Bearing -> Integer
convertB N = 1
convertB E = 4
convertB S = 2
convertB W = 3


