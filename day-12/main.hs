{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
import Useful
import System.IO
import Data.String
import Data.List
import System.CPUTime

data Vector = Vector {x::Integer, y::Integer, z::Integer} deriving (Show, Eq)
data Moon = Moon {pos::Vector, vel::Vector} deriving (Show, Eq)

main = do
        contents <- readFile "input.txt"
--        contents <- readFile "inputtest.txt"
--        contents <- readFile "inputtest2.txt"
        let lines = toLines contents
            moons = map (initMoon.toVector.fromLine) (init lines)
            moons' = steps 1000 moons
            energy = (sum.(map energyMoon)) moons'
        start <- getCPUTime
        print energy
        middle <- timeDif start
        print $ findRestart moons
        end <- timeDif middle
        putStr ""

funcVec :: (Integer -> Integer -> Integer) -> Vector -> Vector -> Vector
funcVec f (Vector x y z) (Vector x' y' z') = Vector (f x x') (f y y') (f z z')

compInt :: Integer -> Integer -> Integer
compInt x x'
  | x > x' = -1
  | x < x' = 1
  | x == x' = 0

compVec :: Vector -> Vector -> Vector
compVec = funcVec compInt

atVector :: (Integer -> Integer -> Integer) -> Vector -> Integer
atVector f Vector{..} = f x (f y z)

energyMoon :: Moon -> Integer
energyMoon Moon{..} = (atVector absAdd pos) * (atVector absAdd vel)
  where absAdd = \x y -> (abs x) + (abs y)

cmpMoonVec :: Moon -> (Vector -> Integer) -> Moon -> Bool
cmpMoonVec Moon{..} f (Moon p' v') = f pos == f p' && f vel == f v'

findWhen' :: [(Moon -> Bool)] -> [Moon] -> Bool
findWhen' [f] [m] = f m
findWhen' (f:fs) (m:ms) = f m && findWhen' fs ms

findWhen :: Integer -> ([Moon] -> Bool) -> [Moon] -> Integer
findWhen n f ms
  | n > 0 && f ms = n
  | otherwise = findWhen (n + 1) f (step ms)

findWhenSingle :: Integer -> (Integer, Integer, Integer) -> ([Moon] -> Bool, [Moon] -> Bool, [Moon] -> Bool) -> [Moon] -> (Integer, Integer, Integer)
findWhenSingle n vs@(x, y, z) fs@(fx,fy,fz) !ms
  | x > 0 && y > 0 && z > 0 = vs
  | otherwise = findWhenSingle (n + 1) (x', y', z') fs (step ms)
  where helper ff f | f == 0 && ff ms = n
                    | otherwise = f
        x' = helper fx x
        y' = helper fy y
        z' = helper fz z

findRestart :: [Moon] -> Integer
findRestart ms = lcm x_val (lcm y_val z_val)
  where originals = \f -> map (\m -> cmpMoonVec m f) ms
        fs = ((findWhen' (originals x)), findWhen' (originals y), findWhen' (originals z))
        (x_val, y_val, z_val) = findWhenSingle 0 (0,0,0) fs ms
--        findRestart' = \f -> findWhen 0 (findWhen' (originals f)) ms
--        x_val = findRestart' x
--        y_val = findRestart' y
--        z_val = findRestart' z

steps :: Integer -> [Moon] -> [Moon]
steps 0 ms = ms
steps n ms = steps (n - 1) (step ms)

step :: [Moon] -> [Moon]
step ms = map (applyVelocity.adjustVel') ms
  where adjustVel' = flip adjustVel ms

applyVelocity :: Moon -> Moon
applyVelocity Moon{..} = Moon (funcVec (+) pos vel) vel

adjustAllVel :: [Moon] -> [Moon]
adjustAllVel ms = map (flip adjustVel ms) ms

adjustVel :: Moon -> [Moon] -> Moon
adjustVel m [] = m
adjustVel (Moon {..}) ((Moon pos' _):ms) = adjustVel m' ms
  where m' = Moon pos $ funcVec (+) vel (compVec pos pos')

initMoon :: Vector -> Moon
initMoon v = Moon v (Vector 0 0 0)

toVector :: [Integer] -> Vector
toVector (x:y:z:[]) = Vector x y z

toLines :: String -> [String]
toLines str = fromStr (delete '\n' str) '>'

fromLine :: String -> [Integer]
fromLine str = map (\xs -> (read::String->Integer) (xs!!1)) split'
  where split = fromStr (tail str) ','
        split' = map (flip fromStr '=') split
