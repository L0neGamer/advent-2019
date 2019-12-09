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

data Param = Pos | Imm | Rel deriving Show
data Op = Input Param -- Integer
        | Output Param
        | BinOp (Integer -> Integer -> Integer) Param Param -- Integer
        | Halt
        | JmpNZ Param Param -- Integer
        | JmpOZ Param Param -- Integer
        | RelBase Param
type Mem = Map Integer Integer
type ProgramCounter = Integer
type RelativeBase = Integer
type InputVals = [Integer]
type OutputVals = [Integer]
data EndState = Halted | AwaitInput | Running deriving (Show, Eq)
data ProgramState = ProgStat
  { mem            :: Mem
  , programCounter :: ProgramCounter
  , inputVals      :: InputVals
  , outputVals     :: OutputVals
  , relBase        :: RelativeBase
  , endState       :: EndState
  } deriving Show


main = do
        contentsT1 <- readFile "../day-07/testinput.txt"
        contentsT2 <- readFile "test2input.txt"
        contentsT3 <- readFile "test3input.txt"
        contentsT4 <- readFile "test4input.txt"
        contents <- readFile "input.txt"
--        contents <- readFile "jessinput.txt"
        let four = [0..4]
            combos = permutations four
            combos' = permutations [5..9]
--            res = sort $ map (\inp -> (runForAmplifiersPt1 inp 0 contents, inp)) combos
--            res = sort $ map (\inp -> (runForAmplifiers inp 0 contentsT1, inp)) combos
--            res2 = sort $ map (\inp -> (runForAmplifiersPt2 inp [Nothing | _ <- four] 0 0 contents, inp)) combos'
--            res' = runStr contents [4,3,2,1,0]
--        print combos
--        print res'
        start <- getCPUTime
        print $ runForAmplifiersPt1 [4,3,2,1,0] 0 contentsT1
--        print $ runForAmplifiersPt1 [0,1,2,3,4] 0 contentsT2
--        print $ runForAmplifiersPt1 [1,0,4,3,2] 0 contentsT3
        print $ runForAmplifiersPt2 [9,8,7,6,5] [Nothing | _ <- four] 0 0 contentsT4
--        print $ runStr contentsT1 [1..10]
--        print $ runStr contentsT2 [1..10]
--        print $ runStr contentsT3 [1..10]
--        print $ runStr contents [1]
        middle <- timeDif start
--        print $ last res2
        end <- timeDif middle
        print $ ""

getOutput :: ProgramState -> Integer
getOutput ps = head $ outputVals ps

runForAmplifiersPt1 :: [Integer] -> Integer -> String -> Integer
runForAmplifiersPt1 [] prev _ = prev
runForAmplifiersPt1 inp prev str = runForAmplifiersPt1 (tail inp) (getOutput ps) str
  where ps = runStr str (head inp:[prev])

replaceAt n xs x = fst splitLst ++ [x] ++ (tail $ snd splitLst)
  where splitLst = splitAt (fromInteger n) xs

checkProgress :: [Maybe ProgramState] -> Bool
checkProgress (Just (ProgStat _ _ _ _ _ Halted):xs) = checkProgress xs
checkProgress [] = True
checkProgress xs = False

runForAmplifiersPt2 :: [Integer] -> [Maybe ProgramState] -> Integer -> Integer -> String -> Integer
runForAmplifiersPt2 phases pss phaseIndex prev str
  | checkProgress newPss = outVal
  | otherwise = runForAmplifiersPt2 phases newPss nextIndex outVal str
  where maybePs = pss!! (fromInteger phaseIndex)
        ps | isNothing maybePs = runStr str ((phases!!(fromInteger phaseIndex)):[prev])
           | otherwise = runProg (setEndState (setInput (fromJust maybePs) [prev]) Running)
        outVal = getOutput ps
        newPss = replaceAt phaseIndex pss (Just ps)
        nextIndex = (mod (phaseIndex + 1) (toInteger $ length phases))

runStr :: String -> [Integer] -> ProgramState
runStr str inputs = runProg $ ProgStat (fromInput str) 0 inputs [] 0 Running

runProg :: ProgramState -> ProgramState
runProg ps@(ProgStat mem pc _ _ _ Running) = runProg (runOp op ps)
  where op = parseOp $ (show $ mem Map.! pc)
runProg ps = ps

runBinOp :: (Integer -> Integer -> Integer) -> Param -> Param -> ProgramState -> ProgramState
runBinOp f p1 p2 (ProgStat mem pc inp out rel _) = ProgStat mem' (pc + 4) inp out rel Running
  where x = getItem p1 (pc + 1) mem rel
        y = getItem p2 (pc + 2) mem rel
        ins = getItem Imm (pc + 3) mem rel
        mem' = Map.insert ins (f x y) mem

runJmp :: (Integer -> Bool) -> Param -> Param -> ProgramState -> ProgramState
runJmp f p1 p2 ps@(ProgStat mem pc _ _ rel _) = setProgramCounter ps pc'
  where pc' | f (getItem p1 (pc + 1) mem rel) = getItem p2 (pc + 2) mem rel
            | otherwise = pc + 3

boolFToInteger f a b = toInteger $ fromEnum (f a b)

setInput (ProgStat mem pc _ out rel es) inp = ProgStat mem pc inp out rel es
setEndState (ProgStat mem pc inp out rel _) es = ProgStat mem pc inp out rel es
setProgramCounter (ProgStat mem _ inp out rel es) pc = ProgStat mem pc inp out rel es
--setRelativeBase (ProgStat mem pc inp out _ es) rel = ProgStat mem pc inp out rel es

runOp :: Op -> ProgramState -> ProgramState
runOp (BinOp f p1 p2) ps = runBinOp f p1 p2 ps
runOp (JmpNZ p1 p2)   ps = runJmp (/= 0) p1 p2 ps
runOp (JmpOZ p1 p2)   ps = runJmp (== 0) p1 p2 ps
runOp (Halt)          ps = setEndState ps Halted
runOp (Output p1)     (ProgStat mem pc inp out rel _) = ProgStat mem (pc + 2) inp ((getItem p1 (pc + 1) mem rel):out) rel Running
runOp (Input p1)      ps@(ProgStat _ _ [] _ _ _) = setEndState ps AwaitInput
runOp (Input p1)      (ProgStat mem pc (i:inp) out rel _) = ProgStat mem' (pc + 2) inp out rel Running
  where mem' = setItem p1 (pc+1) i mem rel
runOp (RelBase p1)    (ProgStat mem pc inp out rel _) = ProgStat mem (pc + 2) inp out (rel + relDif) Running
  where relDif = getItem p1 (pc + 1) mem rel

setItem :: Param -> ProgramCounter -> Integer -> Mem -> RelativeBase -> Mem
setItem Imm pc val mem rel = Map.insert (getItem Imm pc mem rel) val mem
setItem Pos pc val mem rel = Map.insert (getItem Pos pc mem rel) val mem
setItem Rel pc val mem rel = Map.insert (rel + getItem Imm pc mem rel) val mem

getItem :: Param -> ProgramCounter -> Mem -> RelativeBase -> Integer
getItem Imm i mem _  = mem !? i
getItem Pos i mem _  = mem !? (mem !? i)
getItem Rel i mem rb = mem !? (rb + (mem !? i))

(!?) :: (Ord k, Num v) => Map k v -> k -> v
mem !? key
  | key `Map.member` mem = mem Map.! key
  | otherwise = 0

fromInput :: String -> Mem
fromInput str = parseList (fromStr str)

fromStr :: String -> [Integer]
fromStr str = map read stringLst
  where stringLst = wordsWhen (==',') str

enumerate :: [a] -> [(Integer, a)]
enumerate xs = enumerate' xs 0

enumerate' :: [a] -> Integer -> [(Integer, a)]
enumerate' [] _ = []
enumerate' (x:xs) i = (i, x):enumerate' xs (i+1)

parseList :: [Integer] -> Map Integer Integer
parseList xs = Map.fromList $ enumerate xs

getParamOp :: Char -> Param
getParamOp '0' = Pos
getParamOp '1' = Imm
getParamOp '2' = Rel

parseOp :: String -> Op
parseOp [x]            = parseOp ['0',x]
parseOp [y,x]          = constructOp (read [y,x]) Pos Pos
parseOp (x':y:[x])     = constructOp (read [y,x]) (getParamOp x') Pos
parseOp (x'':x':y:[x]) = constructOp (read [y,x]) (getParamOp x') (getParamOp x'')
parseOp xs             = error $ "parse error on " ++ show xs

constructOp :: Integer -> Param -> Param -> Op
constructOp 1 p1 p2 = BinOp (+) p1 p2
constructOp 2 p1 p2 = BinOp (*) p1 p2
constructOp 7 p1 p2 = BinOp (boolFToInteger (<)) p1 p2
constructOp 8 p1 p2 = BinOp (boolFToInteger (==)) p1 p2
constructOp 3 p1 _  = Input p1
constructOp 4 p1 _  = Output p1
constructOp 5 p1 p2 = JmpNZ p1 p2
constructOp 6 p1 p2 = JmpOZ p1 p2
constructOp 9 p1 _  = RelBase p1
constructOp 99 _ _  = Halt
constructOp x  _ _  = error $ "cons error on " ++ show x
