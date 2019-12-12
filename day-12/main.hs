{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

import Useful
import System.IO
import Data.String
import Debug.Trace
import Data.List

data Vector = Vector {x::Integer, y::Integer, z::Integer} deriving (Show, Eq)
data Moon = Moon {pos::Vector, vel::Vector} deriving (Show, Eq)

main = do
        contents <- readFile "input.txt"
--        contents <- readFile "inputtest.txt"
--        contents <- readFile "inputtest2.txt"
        let lines = toLines contents
            moons = map (initMoon.toVector.fromLine) (init lines)
            moons' = steps 1000 moons
            energy = sum $ map energyMoon moons'
--        print lines
--        print moons
        print moons'
        print energy
        print $ steps' 0 moons moons

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

steps' :: Integer -> [Moon] -> [Moon] -> Integer
steps' !n orig !new
  | orig == new && n > 0 = n
  | otherwise = steps' (n + 1) orig (step new)

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
adjustVel !(Moon {..}) ((Moon pos' _):ms) = adjustVel m' ms
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
