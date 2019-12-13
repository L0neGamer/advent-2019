{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
import Useful
import Data.String
import Data.List
import System.CPUTime
import Debug.Trace

data Vector = Vector {x::Integer, y::Integer, z::Integer} deriving (Show, Eq, Ord)
data Moon = Moon {pos::Vector, vel::Vector} deriving (Show, Eq, Ord)

main = do
        contents <- readFile "input.txt"
        let lines = toLines contents
            moons = map (initMoon.toVector.fromLine) (notEmpty lines)
            moons' = steps 1000 moons
            energy = (sum.(map energyMoon)) moons'
        print energy
        print $ findRestart moons
--        putStr ""

notEmpty :: [[a]] -> [[a]]
notEmpty [] = []
notEmpty ([]:xs) = notEmpty xs
notEmpty (x:xs) = x:notEmpty xs

funcVec :: (Integer -> Integer -> Integer) -> Vector -> Vector -> Vector
funcVec f !(Vector x y z) !(Vector x' y' z') = Vector (f x x') (f y y') (f z z')

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
cmpMoonVec' :: Moon -> (Vector -> Integer) -> Moon -> Bool
cmpMoonVec' Moon{..} f (Moon _ v') = f vel == f v'

findWhen' :: [(Moon -> Bool)] -> [Moon] -> Bool
findWhen' [f] [m] = f m
findWhen' (f:fs) (m:ms) = f m && findWhen' fs ms

findWhen :: Integer -> ([Moon] -> Bool) -> [Moon] -> Integer
findWhen n f ms
  | n > 0 && f ms = n
  | otherwise = findWhen (n + 1) f (step ms)

findWhenSingle :: Integer
               -> (Integer, Integer, Integer)
               -> ([Moon] -> Bool, [Moon] -> Bool, [Moon] -> Bool)
               -> [Moon]
               -> (Integer, Integer, Integer)
findWhenSingle n vs@(x, y, z) fs@(fx,fy,fz) ms
  | x > 0 && y > 0 && z > 0 = vs
  | otherwise = findWhenSingle (n + 1) (x', y', z') fs (step ms)
  where helper ff f | f == 0 && ff ms = n
                    | otherwise = f
        x' = helper fx x
        y' = helper fy y
        z' = helper fz z

findRestart :: [Moon] -> Integer
findRestart ms = trace (show xs) $ 2 * lcm x_val (lcm y_val z_val)
  where originals = \f -> map (\m -> cmpMoonVec' m f) ms
        fs = ((findWhen' (originals x)), findWhen' (originals y), findWhen' (originals z))
        xs@(x_val, y_val, z_val) = findWhenSingle 0 (0,0,0) fs ms
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
adjustVel Moon{..} (m'@(Moon pos' _):ms) = adjustVel m' ms
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

getMoons :: [Integer] -> [Moon]
getMoons [] = []
getMoons (x:y:z:xs) = ((initMoon . toVector) [x,y,z]) : getMoons xs
getMoons xs = []

prime_factors :: Integer -> [Integer]
prime_factors 1 = []
prime_factors i = divisor:next
  where divisor = prime_factors' i primes
        next = prime_factors (div i divisor)
prime_factors' :: Integer -> [Integer] -> Integer
prime_factors' i (p:ps)
  | rem i p == 0 = p
  | otherwise = prime_factors' i ps

primes = 2 : primes'
  where isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
        primes' = 3 : filter (isPrime primes') [5, 7 ..]
