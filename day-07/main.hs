import System.IO
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.List
import Data.Maybe
import System.CPUTime

---- thanks to https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

timeDif :: Integer -> IO Integer
timeDif x = do
             y <- getCPUTime
             print $ (fromIntegral (y-x))/(10^9)
             return y

data Param = Rel | Abs deriving Show
data Op = Input -- Int
        | Output Param
        | BinOp (Int -> Int -> Int) Param Param -- Int
        | Halt
        | JmpNZ Param Param -- Int
        | JmpOZ Param Param -- Int
type Mem = Map Int Int
type ProgramCounter = Int
type InputVals = [Int]
type OutputVals = [Int]
data EndState = Halted | AwaitInput | Running deriving (Show, Eq)
data ProgramState = ProgStat
  { mem            :: Mem
  , programCounter :: ProgramCounter
  , inputVals      :: InputVals
  , outputVals     :: OutputVals
  , endState       :: EndState
  } deriving Show


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

getOutput :: ProgramState -> Int
getOutput ps = head $ outputVals ps

runForAmplifiersPt1 :: [Int] -> Int -> String -> Int
runForAmplifiersPt1 [] prev _ = prev
runForAmplifiersPt1 inp prev str = runForAmplifiersPt1 (tail inp) (getOutput ps) str
  where ps = runStr str (head inp:[prev])

replaceAt n xs x = fst splitLst ++ [x] ++ (tail $ snd splitLst)
  where splitLst = splitAt n xs

checkProgress :: [Maybe ProgramState] -> Bool
checkProgress (Just (ProgStat _ _ _ _ Halted):xs) = checkProgress xs
checkProgress [] = True
checkProgress xs = False

runForAmplifiersPt2 :: [Int] -> [Maybe ProgramState] -> Int -> Int -> String -> Int
runForAmplifiersPt2 phases pss phaseIndex prev str
  | checkProgress newPss = outVal
  | otherwise = runForAmplifiersPt2 phases newPss nextIndex outVal str
  where maybePs = pss!!phaseIndex
        ps | isNothing maybePs = runStr str ((phases!!phaseIndex):[prev])
           | otherwise = runProg (setEndState (setInput (fromJust maybePs) [prev]) Running)
        outVal = getOutput ps
        newPss = replaceAt phaseIndex pss (Just ps)
        nextIndex = mod (phaseIndex + 1) (length phases)

runStr :: String -> [Int] -> ProgramState
runStr str inputs = runProg $ ProgStat (fromInput str) 0 inputs [] Running

runProg :: ProgramState -> ProgramState
runProg ps@(ProgStat mem pc _ _ Running) = runProg (runOp op ps)
  where op = parseOp $ (show $ mem Map.! pc)
runProg ps = ps

runBinOp :: (Int -> Int -> Int) -> Param -> Param -> ProgramState -> ProgramState
runBinOp f p1 p2 (ProgStat mem pc inp out _) = ProgStat mem' (pc + 4) inp out Running
  where x = getItem p1 (pc + 1) mem
        y = getItem p2 (pc + 2) mem
        ins = getItem Abs (pc + 3) mem
        mem' = Map.insert ins (f x y) mem

runJmp :: (Int -> Bool) -> Param -> Param -> ProgramState -> ProgramState
runJmp f p1 p2 ps@(ProgStat mem pc inp out _) = setProgramCounter ps pc'
  where pc' | f (getItem p1 (pc + 1) mem) = getItem p2 (pc + 2) mem
            | otherwise = pc + 3

boolFToInt f a b = fromEnum (f a b)

setInput (ProgStat mem pc _ out es) inp = ProgStat mem pc inp out es
setEndState (ProgStat mem pc inp out _) es = ProgStat mem pc inp out es
setProgramCounter (ProgStat mem _ inp out es) pc = ProgStat mem pc inp out es

runOp :: Op -> ProgramState -> ProgramState
runOp (BinOp f p1 p2) ps = runBinOp f p1 p2 ps
runOp (JmpNZ p1 p2)   ps = runJmp (/= 0) p1 p2 ps
runOp (JmpOZ p1 p2)   ps = runJmp (== 0) p1 p2 ps
runOp (Halt)          ps = setEndState ps Halted
runOp (Output p1)     (ProgStat mem pc inp out _) = ProgStat mem (pc + 2) inp ((getItem p1 (pc + 1) mem):out) Running
runOp (Input)         ps@(ProgStat _ _ [] _ _) = setEndState ps AwaitInput
runOp (Input)         (ProgStat mem pc (i:inp) out _) = ProgStat mem' (pc + 2) inp out Running
  where mem' = Map.insert (getItem Abs (pc + 1) mem) i mem

getItem :: Param -> Int -> Map Int Int -> Int
getItem Abs i map = map Map.! i
getItem Rel i map = map Map.! (map Map.! i)

fromInput :: String -> Map Int Int
fromInput str = parseList (fromStr str)

fromStr :: String -> [Int]
fromStr str = map read stringLst
  where stringLst = wordsWhen (==',') str

enumerate :: [a] -> [(Int, a)]
enumerate xs = enumerate' xs 0

enumerate' :: [a] -> Int -> [(Int, a)]
enumerate' [] _ = []
enumerate' (x:xs) i = (i, x):enumerate' xs (i+1)

parseList :: [Int] -> Map Int Int
parseList xs = Map.fromList $ enumerate xs

getParamOp :: Char -> Param
getParamOp '0' = Rel
getParamOp '1' = Abs

parseOp :: String -> Op
parseOp [x]            = parseOp ['0',x]
parseOp [y,x]          = constructOp (read [y,x]) Rel Rel
parseOp (x':y:[x])     = constructOp (read [y,x]) (getParamOp x') Rel
parseOp (x'':x':y:[x]) = constructOp (read [y,x]) (getParamOp x') (getParamOp x'')
parseOp xs             = error $ "parse error on " ++ show xs

constructOp :: Int -> Param -> Param -> Op
constructOp 1 p1 p2 = BinOp (+) p1 p2
constructOp 2 p1 p2 = BinOp (*) p1 p2
constructOp 7 p1 p2 = BinOp (boolFToInt (<)) p1 p2
constructOp 8 p1 p2 = BinOp (boolFToInt (==)) p1 p2
constructOp 3 _  _  = Input
constructOp 4 p1 _  = Output p1
constructOp 5 p1 p2 = JmpNZ p1 p2
constructOp 6 p1 p2 = JmpOZ p1 p2
constructOp 99 _ _  = Halt
constructOp x  _ _  = error $ "cons error on " ++ show x
