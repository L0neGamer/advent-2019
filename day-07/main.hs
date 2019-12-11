import System.IO
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.List
import Data.Maybe
import System.CPUTime
import IntCode
import Useful

main = do
        contentsT1 <- readFile "testinput.txt"
        contentsT2 <- readFile "test2input.txt"
        contentsT3 <- readFile "test3input.txt"
        contentsT4 <- readFile "test4input.txt"
        contents <- readFile "input.txt"
--        contents <- readFile "jessinput.txt"
        let four = [0..4]
            combos = permutations four
            combos' = permutations [5..9]
            res = sort $ map (\inp -> (runForAmplifiersPt1 inp 0 contents, inp)) combos
--            res = sort $ map (\inp -> (runForAmplifiers inp 0 contentsT1, inp)) combos
            res2 = sort $ map (\inp -> (runForAmplifiersPt2 inp [Nothing | _ <- four] 0 0 contents, inp)) combos'
--            res' = runStr contents [4,3,2,1,0]
--        print combos
--        print res'
        start <- getCPUTime
--        print $ runForAmplifiersPt1 [4,3,2,1,0] 0 contentsT1
--        print $ runForAmplifiersPt1 [0,1,2,3,4] 0 contentsT2
--        print $ runForAmplifiersPt1 [1,0,4,3,2] 0 contentsT3
--        print $ runForAmplifiersPt2 [9,8,7,6,5] [Nothing | _ <- four] 0 0 contentsT4
        print $ last res
        middle <- timeDif start
        print $ last res2
        end <- timeDif middle
        print $ ""

getOutput :: ProgramState -> Integer
getOutput ps = head $ out ps

runForAmplifiersPt1 :: [Integer] -> Integer -> String -> Integer
runForAmplifiersPt1 [] prev _ = prev
runForAmplifiersPt1 inp prev str = runForAmplifiersPt1 (tail inp) (getOutput ps) str
  where ps = runStr str (head inp:[prev])

checkProgress :: [Maybe ProgramState] -> Bool
checkProgress (Just (ProgStat{es=Halted}):xs) = checkProgress xs
checkProgress [] = True
checkProgress xs = False

runForAmplifiersPt2 :: [Integer] -> [Maybe ProgramState] -> Integer -> Integer -> String -> Integer
runForAmplifiersPt2 phases pss phaseIndex prev str
  | checkProgress newPss = outVal
  | otherwise = runForAmplifiersPt2 phases newPss nextIndex outVal str
  where maybePs = pss!!(fromIntegral phaseIndex)
        ps | isNothing maybePs = runStr str ((phases!!(fromIntegral phaseIndex)):[prev])
           | otherwise = runProg (setEndState (setInput (fromJust maybePs) [prev]) Running)
        newPss = replaceAt phaseIndex pss (Just ps)
        outVal = getOutput ps
        nextIndex = toInteger $ mod (fromIntegral $ phaseIndex + 1) (length phases)
