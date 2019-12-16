{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
import Useful
import IntCode
import Data.String
import Data.List
import Debug.Trace
import Data.Maybe
import qualified Data.Map as M

type Signal = [Integer]
type Phase = [Integer]

main = do
        contents <- Useful.readFile "input.txt"
        let res = getDigits $ read contents
            t1 = (getDigits 12345678)
            t2 = getDigits 80871224585914546619083218645595
            t3 = getDigits 69317163492948606335995924319873
--        print $ runNPhases t1 [1,0,-1,0] 1
--        print $ take 8 $ runNPhases t2 [1,0,-1,0] 100
--        print $ take 8 $ runNPhases t3 [1,0,-1,0] 100
--        print $ take 8 $ runNPhases res 100 -- 29795507
        print $ part2 (tail $ getDigits 103036732577212944063491565474664) 100 -- 84462026
        print $ part2 (tail $ getDigits 102935109699940807407585447034323) 100 -- 78725270
        print $ part2 (tail $ getDigits 103081770884921959731165446850517) 100 -- 53553731
        print $ part2 res 100 -- not 26261059
        putStr ""

getLastDigit :: Integer -> Integer
getLastDigit i = mod (abs i) 10

getDigits :: Integer -> [Integer]
getDigits inp = res
  where strInp = show inp
        res = map (\c -> read [c]) strInp

toInteger' :: (Integral b, Read b) => [Integer] -> b
toInteger' xs = read $ concat $ map show xs

part2 :: Signal -> Integer -> Signal
part2 sig n = res'
  where sig' = concat [sig | _ <-[1..10000]]
        offset = toInteger' $ take 7 sig
        sig'' = drop offset sig'
        res = doNPhases sig'' n offset (length sig')
        res' = take 8 res

runNPhases :: Signal -> Integer -> Signal
runNPhases sig n = doNPhases sig n 0 (length sig)

doNPhases :: Signal -> Integer -> Int -> Int -> Signal
doNPhases !sig 0 _ _ = sig
doNPhases !sig n startPos sigSize = doNPhases next (n - 1) startPos sigSize
  where next = (fromSignal' sig startPos sigSize)

fromSignal :: Signal -> Signal
fromSignal sig = fromSignal' sig 0 (length sig)

sumEachTail :: Signal -> Signal
sumEachTail (ss) = reverse $ map getLastDigit (scanl' (+) r rev) -- thanks ryan
  where (r:rev) = reverse ss

fromSignal' :: Signal -> Int -> Int -> Signal
fromSignal' sig signalIndex sigLen
  | signalIndex >= length sig = sumEachTail sig
  | signalIndex < sigLen = currRes:next
  | otherwise = []
  where currRes = getLastDigit $ applyPatternTo sig signalIndex sigLen
        next = fromSignal' (tail sig) (signalIndex + 1) sigLen

applyPatternTo :: Signal -> Int -> Int -> Integer
applyPatternTo [] _ _ = 0
applyPatternTo sig sigIndex sigLen = (a + b) + applyPatternTo (drop (toTake * 4) sig) sigIndex sigLen
  where toTake = sigIndex + 1
        a = sum (take toTake sig)
        b = - (sum $ take toTake (drop (2*toTake) sig))


getPhases :: Phase -> Integer -> [Phase]
getPhases phase 0 = []
getPhases phase i = infPhase : getPhases phase (i - 1)
  where infPhase = infinitePhase (expandPhase phase i)

infinitePhase :: (Phase -> Phase) -> Phase
infinitePhase phaseFunc = phaseFunc (infinitePhase phaseFunc)

expandPhase :: Phase -> Integer -> (Phase -> Phase)
expandPhase [] _ = \p' -> p'
expandPhase (p:ps) i = \p' -> expandThis (expandNext p')
  where expandThis = expandPhase' p i
        expandNext = expandPhase ps i

expandPhase' :: Integer -> Integer -> (Phase -> Phase)
expandPhase' p 0 = \p' -> p'
expandPhase' p i = \p' -> p:(expandPhase' p (i-1)) p'
