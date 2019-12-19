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
        contents <- Useful.readFile "input.txt"
--        contents <- Useful.readFile "inputskumby.txt"
        let res = findTractorPoints contents
            res2 = findTractorPointsPt2 contents
            ps = flip setEndState Running $ runStr contents []
            top = followTopEdge ps (100,0)
            tm = M.fromList $ map (\p -> (p,(Path,p))) top
        print res
        print res2
--        putStr $ drawTileMap tm
--        print top

findTractorPoints :: String -> Integer
findTractorPoints str = sum [head (out (runProg (flip setEndState Running $ setInput ps [x,y]))) | x<-[0..49],y<-[0..49]]
  where ps = runStr str []

infiniteGrid :: [Point]
infiniteGrid = concat infiniteGrid'

infiniteGrid' :: [[Point]]
infiniteGrid' = [[(x,t-x)|x<-[0..t]] | t<-[0..]]

isValid' :: ProgramState -> Point -> Bool
isValid' ps (x,y) = 1 == (head $ out $ runProg $ setInput ps [x,y])

isValid :: [Point] -> Integer -> Point -> Bool
isValid xs i p@(x,y) = (x+i,y) `elem` maxBound && (x,y+i) `elem` maxBound
  where minBound = dropWhile (/=p) xs
        maxBound = takeWhile (\(x',y') -> x' < x+i || y' < y+i) minBound

--helper :: ProgramState -> Integer -> Integer -> [Point]
--helper ps min_x min_y = concat [xs, ys, trace (show xs ++ show ys) (helper ps (min_x + 1) (min_y + 1))]
--  where xs = takeWhile (isValid' ps) [(x,min_y) | x <- [min_x..]]
--        ys = takeWhile (isValid' ps) [(min_x,y) | y <- [min_y..]]
--        around = filter (isValid' ps) infiniteGrid

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

finder :: ProgramState -> [Point] -> ([Point],Maybe Point)
finder ps !xs = (valid, next)
  where rest = dropWhile (\p@(x,y) -> (not (isValid' ps p))) xs
        valid = takeWhile (isValid' ps) rest
        potentialNext (x,y) = [(x,y+1),(x+1,y),(x+1,y+1)]
        next = maybeHead valid >>= \p -> maybeHead $ filter (isValid' ps) (potentialNext p)

finder' :: ProgramState -> [[Point]] -> [Point]
finder' ps (x:y:xs)
  | isJust begin = concat [pts, finder' ps (droppedY:xs)]
  | otherwise = concat [pts, finder' ps (y:xs)]
  where (pts,begin) = finder ps x
        droppedY = dropWhile (/=(fromJust begin)) y

followTopEdge :: ProgramState -> Point -> [Point]
followTopEdge ps p@(x,y)
  | not (isValid' ps p) = followTopEdge ps (x,y+1)
  | isValidTopEdge ps p = [p]
  | otherwise = p:followTopEdge ps (x+1,y)

isValidTopEdge :: ProgramState -> Point -> Bool
isValidTopEdge ps p@(x,y) = isValid' ps (x-99,y) && isValid' ps (x,y+99) && isValid' ps (x-99,y+99)

findTractorPointsPt2 :: String -> Point
findTractorPointsPt2 str = (x-99,y)
--findTractorPointsPt2 str = head $ filter (isValid inCone' 100) inCone'
--findTractorPointsPt2 str = [(x,y) | x<-[1..],y<-[1..], isValid' ps (x,y) || ]
  where ps = flip setEndState Running $ runStr str []
        (x,y) = last $ followTopEdge ps (1000,500)
--        inCone = helper ps 6 5
--        inCone = filter (isValid' ps) infiniteGrid
--        inCone = concat $ map (finder ps) infiniteGrid'
        inCone = finder' ps infiniteGrid'
        tileMap = M.fromList $ map (\p -> (p,(Path,p))) (takeWhile (\p -> not $ isValid inCone 10 p) inCone)
        inCone' = trace (drawTileMap tileMap) inCone
