{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
import System.IO
import qualified Data.Map.Strict as M
import Data.List
import IntCode
import Useful
import System.CPUTime

main = do
        contents <- readFile "input.txt"
        let init = runStr contents []
            pc = paintingChassis init (M.insert (0,0) (1,0) M.empty) (0,0) N
        start <- getCPUTime
        print pc
        print $ paintingCount pc
        middle <- timeDif start
        putStrLn $ drawPaintedMap pc
        end <- timeDif middle
        putStr ""

bearingAntiClockwise :: Bearing -> Bearing
bearingAntiClockwise N = W
bearingAntiClockwise E = N
bearingAntiClockwise S = E
bearingAntiClockwise W = S
bearingClockwise :: Bearing -> Bearing
bearingClockwise N = E
bearingClockwise E = S
bearingClockwise S = W
bearingClockwise W = N

drawInt :: Integer -> Char
drawInt 0 = ' '
drawInt 1 = head "\x2588"

drawPaintedMap :: PaintedMap -> String
drawPaintedMap pm = concat [ getRow y | y <- [max_y,(max_y-1)..min_y]]
  where keys = M.keys pm :: [Point]
        tup_cmp fnc a b = compare (fnc a) (fnc b)
        getMostVal cmp fnc ks = fnc $ cmp (tup_cmp fnc) ks
        max_x = getMostVal maximumBy fst keys
        min_x = getMostVal minimumBy fst keys
        max_y = getMostVal maximumBy snd keys
        min_y = getMostVal minimumBy snd keys
        getRow y = [drawInt (fst (M.findWithDefault (0,0) (x,y) pm)) | x <- [min_x..max_x]] ++ "\n" :: String

paintingCount :: PaintedMap -> Int
paintingCount pm = length $ filter (\(_,(_,x)) -> x > 0) $ M.toList pm

paintingChassis :: ProgramState -> PaintedMap -> Point -> Bearing -> PaintedMap
paintingChassis ps@(ProgStat _ _ _ _ _ Halted) pm p b = pm
paintingChassis !ps pm !p !b = paintingChassis ps' pm' p' b'
  where currentTile@(c, num) = M.findWithDefault (0,0) p pm
        ps' = runProg $ setEndState (setInput ps [c]) Running
        (dir:c':xs) = out ps'
        pm' = M.insert p (c', num + 1) pm
        bearingConvert | dir == 0 = bearingAntiClockwise
                       | dir == 1 = bearingClockwise
        b' = bearingConvert b
        p' = moveInBearing p b'
