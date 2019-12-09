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
data StoredFunc = SF2 (Integer -> Integer -> Integer) String | SF1 (Integer -> Integer) String
data Op = Input Param
        | Output Param
        | BinOp StoredFunc Param Param Param
        | Halt
        | JmpIf StoredFunc Param Param
        | RelBase Param
        deriving Show
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

instance Show StoredFunc where
  show (SF2 _ str) = "(SF2 " ++ str ++ ")"
  show (SF1 _ str) = "(SF1 " ++ str ++ ")"

callFunc :: StoredFunc -> Integer -> Integer -> Integer
callFunc (SF2 f _) a b = f a b
callFunc (SF1 f _) a _ = f a

main = do
        contents7T1 <- readFile "../day-07/testinput.txt"
        contentsT1 <- readFile "testinput.txt"
        contentsT2 <- readFile "test2input.txt"
        contentsT3 <- readFile "test3input.txt"
        contentsT4 <- readFile "test4input.txt"
        contents <- readFile "input.txt"
        let four = [0..4]
            combos = permutations four
            combos' = permutations [5..9]
        start <- getCPUTime
        print $ runStr contents [2]
        middle <- timeDif start
        end <- timeDif middle
        print $ ""

replaceAt n xs x = fst splitLst ++ [x] ++ (tail $ snd splitLst)
  where splitLst = splitAt (fromInteger n) xs

runStr :: String -> [Integer] -> ProgramState
runStr str inputs = runProg $ ProgStat (fromInput str) 0 inputs [] 0 Running

runProg :: ProgramState -> ProgramState
runProg ps@(ProgStat mem pc _ _ _ Running) = runProg (runOp op ps)
  where op = parseOp $ (show $ mem Map.! pc)
runProg ps = ps

runBinOp :: StoredFunc -> Param -> Param -> Param -> ProgramState -> ProgramState
runBinOp f p1 p2 p3 (ProgStat mem pc inp out rel _) = ProgStat mem' (pc + 4) inp out rel Running
  where x = getItem p1 (pc + 1) mem rel
        y = getItem p2 (pc + 2) mem rel
        mem' = setItem p3 (pc + 3) (callFunc f x y) mem rel

runJmp :: StoredFunc -> Param -> Param -> ProgramState -> ProgramState
runJmp f p1 p2 ps@(ProgStat mem pc _ _ rel _) = setProgramCounter ps pc'
  where pc' | 1 == (callFunc f (getItem p1 (pc + 1) mem rel) 0) = getItem p2 (pc + 2) mem rel
            | otherwise = pc + 3

boolFToInteger f a b = toInteger $ fromEnum (f a b)

setInput (ProgStat mem pc _ out rel es) inp = ProgStat mem pc inp out rel es
setEndState (ProgStat mem pc inp out rel _) es = ProgStat mem pc inp out rel es
setProgramCounter (ProgStat mem _ inp out rel es) pc = ProgStat mem pc inp out rel es

runOp :: Op -> ProgramState -> ProgramState
runOp (BinOp f p1 p2 p3) ps = runBinOp f p1 p2 p3 ps
runOp (JmpIf f p1 p2)    ps = runJmp f p1 p2 ps
runOp (Halt)             ps = setEndState ps Halted
runOp (Output p1)        (ProgStat mem pc inp out rel _) = ProgStat mem (pc + 2) inp ((getItem p1 (pc + 1) mem rel):out) rel Running
runOp (Input p1)         ps@(ProgStat _ _ [] _ _ _) = setEndState ps AwaitInput
runOp (Input p1)         (ProgStat mem pc (i:inp) out rel _) = ProgStat mem' (pc + 2) inp out rel Running
  where mem' = setItem p1 (pc + 1) i mem rel
runOp (RelBase p1)       (ProgStat mem pc inp out rel _) = ProgStat mem (pc + 2) inp out (rel + relDif) Running
  where relDif = getItem p1 (pc + 1) mem rel

setItem :: Param -> ProgramCounter -> Integer -> Mem -> RelativeBase -> Mem
setItem Rel pc val mem rel = Map.insert rel' val mem
  where rel' = rel + getItem Imm pc mem rel
setItem Imm pc val mem rel = Map.insert (getItem Imm pc mem rel) val mem

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

simplifyParam :: Param -> Param
simplifyParam Rel = Rel
simplifyParam _ = Imm

parseOp :: String -> Op
parseOp [x]                 = parseOp ['0',x]
parseOp [y,x]               = constructOp (read [y,x]) Pos Pos Imm
parseOp (x':y:[x])          = constructOp (read [y,x]) (getParamOp x') Pos Imm
parseOp (x'':x':y:[x])      = constructOp (read [y,x]) (getParamOp x') (getParamOp x'') Imm
parseOp (x''':x'':x':y:[x]) = constructOp (read [y,x]) (getParamOp x') (getParamOp x'') (simplifyParam $ getParamOp x''')
parseOp xs                  = error $ "parse error on " ++ show xs

constructOp :: Integer -> Param -> Param -> Param -> Op
constructOp 1 p1 p2 p3 = BinOp (SF2 (+) "+") p1 p2 p3
constructOp 2 p1 p2 p3 = BinOp (SF2 (*) "*") p1 p2 p3
constructOp 7 p1 p2 p3 = BinOp (SF2 (boolFToInteger (<)) "<") p1 p2 p3
constructOp 8 p1 p2 p3 = BinOp (SF2 (boolFToInteger (==)) "==") p1 p2 p3
constructOp 3 p1 _  _  = Input (simplifyParam p1)
constructOp 4 p1 _  _  = Output p1
constructOp 5 p1 p2 _  = JmpIf (SF1 (boolFToInteger (/=) 0) "/=0") p1 p2
constructOp 6 p1 p2 _  = JmpIf (SF1 (boolFToInteger (==) 0) "==0") p1 p2
constructOp 9 p1 _  _  = RelBase p1
constructOp 99 _ _  _  = Halt
constructOp x  _ _  _  = error $ "cons error on " ++ show x
