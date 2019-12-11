{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
import System.IO
import Control.Monad
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe
import System.CPUTime
import Debug.Trace

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

fromStr :: String -> Char -> [String]
fromStr str c = wordsWhen (==c) str

enumerate :: [a] -> [(Integer, a)]
enumerate xs = enumerate' xs 0

enumerate' :: [a] -> Integer -> [(Integer, a)]
enumerate' [] _ = []
enumerate' (x:xs) i = (i, x):enumerate' xs (i+1)

replaceAt n xs x = fst splitLst ++ [x] ++ (tail $ snd splitLst)
  where splitLst = splitAt (fromInteger n) xs

data Param = Pos | Imm | Rel deriving Show
data StoredFunc = SF2 (Integer -> Integer -> Integer) String | SF1 (Integer -> Integer) String
data Op = Input Param
        | Output Param
        | BinOp StoredFunc Param Param Param
        | Halt
        | JmpIf StoredFunc Param Param
        | RelBase Param
        deriving Show
type Mem = M.Map Integer Integer
type ProgramCounter = Integer
type RelativeBase = Integer
type InputVals = [Integer]
type OutputVals = [Integer]
data EndState = Halted | AwaitInput | Running deriving (Show, Eq)
data ProgramState = ProgStat
  { mem            :: !Mem
  , programCounter :: !ProgramCounter
  , inputVals      :: !InputVals
  , outputVals     :: !OutputVals
  , relBase        :: !RelativeBase
  , endState       :: !EndState
  } deriving Show
type Point = (Integer, Integer)
type TileColour = Integer
type Tile = (TileColour, Integer)
type PaintedMap = M.Map Point Tile
data Bearing = N | S | E | W

instance Show StoredFunc where
  show (SF2 _ str) = "(SF2 " ++ str ++ ")"
  show (SF1 _ str) = "(SF1 " ++ str ++ ")"

callFunc :: StoredFunc -> Integer -> Integer -> Integer
callFunc (SF2 f _) a b = f a b
callFunc (SF1 f _) a _ = f a

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
paintingChassis ps@(ProgStat mem pc inp out rel Halted) pm p b = pm
paintingChassis !ps pm !p !b = paintingChassis ps' pm' p' b'
  where currentTile@(c, num) = M.findWithDefault (0,0) p pm
        ps' = runProg $ setEndState (setInput ps [c]) Running
        out@(dir:c':xs) = getOutput ps'
        pm' = M.insert p (c', num + 1) pm
        bearingConvert | dir == 0 = bearingAntiClockwise
                       | dir == 1 = bearingClockwise
        b' = bearingConvert b
        p' = moveInBearing p b'

moveInBearing :: Point -> Bearing -> Point
moveInBearing (x, y) N = (x, y+1)
moveInBearing (x, y) E = (x+1, y)
moveInBearing (x, y) W = (x-1, y)
moveInBearing (x, y) S = (x, y-1)

getOutput (ProgStat _ _ _ out _ _) = out

runStr :: String -> [Integer] -> ProgramState
runStr str inputs = runProg $ ProgStat (fromInput str) 0 inputs [] 0 Running

runProg :: ProgramState -> ProgramState
runProg ps@(ProgStat mem pc _ _ _ Running) = runProg (runOp op ps)
  where op = parseOp $ (show $ mem M.! pc)
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
setItem Rel pc val mem rel = M.insert rel' val mem
  where rel' = rel + getItem Imm pc mem rel
setItem Pos pc val mem rel = M.insert index val mem
  where index = getItem Imm pc mem rel

getItem :: Param -> ProgramCounter -> Mem -> RelativeBase -> Integer
getItem Imm i mem _  = mem !? i
getItem Pos i mem _  = mem !? (mem !? i)
getItem Rel i mem rb = mem !? (rb + (mem !? i))

(!?) :: (Ord k, Num v) => M.Map k v -> k -> v
mem !? key = M.findWithDefault 0 key mem

fromInput :: String -> Mem
fromInput str = parseList (fromStr str ',')

parseList :: [String] -> Mem
parseList xs = M.fromList $ enumerate $ map read xs

getParamOp :: Char -> Param
getParamOp '0' = Pos
getParamOp '1' = Imm
getParamOp '2' = Rel

simplifyParam :: Param -> Param
simplifyParam Imm = error "tried to simplify an immediate access param"
simplifyParam x = x

parseOp :: String -> Op
parseOp [x]                 = parseOp ['0',x]
parseOp [y,x]               = constructOp (read [y,x]) Pos Pos Pos
parseOp (x':y:[x])          = constructOp (read [y,x]) (getParamOp x') Pos Pos
parseOp (x'':x':y:[x])      = constructOp (read [y,x]) (getParamOp x') (getParamOp x'') Pos
parseOp (x''':x'':x':y:[x]) = constructOp (read [y,x]) (getParamOp x') (getParamOp x'') (simplifyParam $ getParamOp x''')
parseOp xs                  = error $ "parse error on " ++ show xs

constructOp :: Integer -> Param -> Param -> Param -> Op
constructOp 1 p1 p2 p3 = BinOp (msf "+") p1 p2 p3
constructOp 2 p1 p2 p3 = BinOp (msf "*") p1 p2 p3
constructOp 7 p1 p2 p3 = BinOp (msf "<") p1 p2 p3
constructOp 8 p1 p2 p3 = BinOp (msf "==") p1 p2 p3
constructOp 5 p1 p2 _  = JmpIf (msf "/=0") p1 p2
constructOp 6 p1 p2 _  = JmpIf (msf "==0") p1 p2
constructOp 3 p1 _  _  = Input (simplifyParam p1)
constructOp 4 p1 _  _  = Output p1
constructOp 9 p1 _  _  = RelBase p1
constructOp 99 _ _  _  = Halt
constructOp x  _ _  _  = error $ "cons error on " ++ show x

msf = makeStoredFunc

makeStoredFunc :: String -> StoredFunc
makeStoredFunc "+" = SF2 (+) "+"
makeStoredFunc "*" = SF2 (*) "*"
makeStoredFunc "<" = SF2 (boolFToInteger (<)) "<"
makeStoredFunc "==" = SF2 (boolFToInteger (==)) "=="
makeStoredFunc "/=0" = SF1 (boolFToInteger (/=) 0) "/=0"
makeStoredFunc "==0" = SF1 (boolFToInteger (==) 0) "==0"
