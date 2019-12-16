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
        print $ take 8 $ runNPhases res 100 -- 29795507
        print $ part2 (tail $ getDigits 103036732577212944063491565474664) 100 -- 84462026
        print $ part2 (tail $ getDigits 102935109699940807407585447034323) 100 -- 78725270
        print $ part2 (tail $ getDigits 103081770884921959731165446850517) 100 -- 53553731
        print $ part2 res 100
        putStr ""

getLastDigit :: Integer -> Integer
getLastDigit i = mod (abs i) 10

getIntegerSize :: Integer -> Integer
getIntegerSize inp = floor $ logBase 10 (fromIntegral inp)

getDigits :: Integer -> [Integer]
getDigits inp = res
  where strInp = show inp
        res = map (\c -> read [c]) strInp
--getDigits inp = [getLastDigit (div inp num) | num <- divisors]
--  where inpLength = getIntegerSize inp
--        divisors = map floor [10**(fromIntegral d) | d <- [inpLength,(inpLength-1)..0]]

--part2 :: Signal -> Phase -> Integer -> Signal
part2 sig n = res'
  where sig' = concat [sig | _ <-[0..10000]]
        offset = read $ concat $ map (show::(Integer -> String)) $ take 7 sig :: Int
--        phases = trace (show (length sig') ++ " " ++ show (length (drop offset sig'))) reverse $ getPhases phase (fromIntegral (length sig'))
--        res = doNPhases (drop offset sig') (map (drop offset) phases) n
        sig'' = drop (offset) sig'
        res = trace (show (length sig'' < (div (length sig') 2))) doNPhases sig'' 100 offset (length sig')
        res' = take 8 res

runNPhases :: Signal -> Integer -> Signal
runNPhases sig n = doNPhases sig n 0 (length sig)
--  where phases = (reverse $ getPhases phase (fromIntegral (length sig)))

doNPhases :: Signal -> Integer -> Int -> Int -> Signal
doNPhases !sig 0 _ _ = sig
--doNPhases !sig phases n startPos sigSize = trace (show (length sig) ++ " " ++ show sigSize ++ " " ++ show n) $ doNPhases next phases (n - 1) startPos sigSize
doNPhases !sig n startPos sigSize = doNPhases next (n - 1) startPos sigSize
  where next = (fromSignal' sig startPos sigSize)

fromSignal :: Signal -> Signal
fromSignal sig = fromSignal' sig 0 (length sig)

sumEachTail :: Signal -> Signal
--sumEachTail [] = []
--sumEachTail (s:ss) = flip (:) (sumEachTail ss) $! ((getLastDigit.sum) (s:ss))
sumEachTail (ss) = reverse $ map getLastDigit (scanl' (+) r rev)
  where (r:rev) = reverse ss

fromSignal' :: Signal -> Int -> Int -> Signal
--fromSignal' [] _ _ _ = error "at the disco"
fromSignal' sig signalIndex sigLen
  | signalIndex == sigLen = []
  | signalIndex >= length sig = id $! sumEachTail sig
  | signalIndex < sigLen = currRes:next
  | otherwise = []
  where currRes = getLastDigit $ applyPatternTo sig signalIndex sigLen
--        currRes = getLastDigit.sum $ zipWith (*) sig (phases!!signalIndex)
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
