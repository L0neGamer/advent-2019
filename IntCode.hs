{-# LANGUAGE RecordWildCards #-}

module IntCode where
import qualified Data.Map.Strict as M
import Useful

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
  { mem :: !Mem
  , pc  :: !ProgramCounter
  , inp :: !InputVals
  , out :: !OutputVals
  , rel :: !RelativeBase
  , es  :: !EndState
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

runStr :: String -> [Integer] -> ProgramState
runStr str inputs = runProg $ initState str inputs

initState :: String -> [Integer] -> ProgramState
initState str inputs = ProgStat (fromInput str) 0 inputs [] 0 Running

consPSFromMem :: Mem -> ProgramState
consPSFromMem mem = ProgStat mem 0 [] [] 0 Running

runProg :: ProgramState -> ProgramState
runProg ps@(ProgStat {es=Running,..}) = runProg (runOp op ps)
  where op = parseOp $ (show $ mem M.! pc)
runProg ps = ps

runBinOp :: StoredFunc -> Param -> Param -> Param -> ProgramState -> ProgramState
runBinOp f p1 p2 p3 ProgStat{..} = ProgStat mem' (pc + 4) inp out rel Running
  where x = getItem p1 (pc + 1) mem rel
        y = getItem p2 (pc + 2) mem rel
        mem' = setItem p3 (pc + 3) (callFunc f x y) mem rel

runJmp :: StoredFunc -> Param -> Param -> ProgramState -> ProgramState
runJmp f p1 p2 ps@ProgStat{..} = setProgramCounter ps pc'
  where pc' | 1 == (callFunc f (getItem p1 (pc + 1) mem rel) 0) = getItem p2 (pc + 2) mem rel
            | otherwise = pc + 3

boolFToInteger f a b = toInteger $ fromEnum (f a b)

setInput ProgStat{..} inp' = ProgStat mem pc inp' out rel es
setEndState ProgStat{..} es' = ProgStat mem pc inp out rel es'
setProgramCounter ProgStat{..} pc' = ProgStat mem pc' inp out rel es
clearBuffs ProgStat{..} = ProgStat mem pc [] [] rel es

runOp :: Op -> ProgramState -> ProgramState
runOp (BinOp f p1 p2 p3) ps = runBinOp f p1 p2 p3 ps
runOp (JmpIf f p1 p2)    ps = runJmp f p1 p2 ps
runOp (Halt)             ps = setEndState ps Halted
runOp (Output p1)        ProgStat{..} = ProgStat mem (pc + 2) inp ((getItem p1 (pc + 1) mem rel):out) rel Running
runOp (Input p1)         ps@ProgStat{inp=[],..} = setEndState ps AwaitInput
runOp (Input p1)         ProgStat{inp=(i:inp),..} = ProgStat mem' (pc + 2) inp out rel Running
  where mem' = setItem p1 (pc + 1) i mem rel
runOp (RelBase p1)       ProgStat{..} = ProgStat mem (pc + 2) inp out (rel + relDif) Running
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
parseOp [y,x]               = constructOp [y,x] Pos Pos Pos
parseOp (x':y:[x])          = constructOp [y,x] (getParamOp x') Pos Pos
parseOp (x'':x':y:[x])      = constructOp [y,x] (getParamOp x') (getParamOp x'') Pos
parseOp (x''':x'':x':y:[x]) = constructOp [y,x] (getParamOp x') (getParamOp x'') (simplifyParam $ getParamOp x''')
parseOp xs                  = error $ "parse error on " ++ show xs

constructOp :: String -> Param -> Param -> Param -> Op
constructOp "01" p1 p2 p3 = BinOp (msf "+") p1 p2 p3
constructOp "02" p1 p2 p3 = BinOp (msf "*") p1 p2 p3
constructOp "07" p1 p2 p3 = BinOp (msf "<") p1 p2 p3
constructOp "08" p1 p2 p3 = BinOp (msf "==") p1 p2 p3
constructOp "05" p1 p2 _  = JmpIf (msf "/=0") p1 p2
constructOp "06" p1 p2 _  = JmpIf (msf "==0") p1 p2
constructOp "03" p1 _  _  = Input (simplifyParam p1)
constructOp "04" p1 _  _  = Output p1
constructOp "09" p1 _  _  = RelBase p1
constructOp "99" _  _  _  = Halt
constructOp x    _  _  _  = error $ "cons error on " ++ show x

msf = makeStoredFunc

makeStoredFunc :: String -> StoredFunc
makeStoredFunc "+" = SF2 (+) "+"
makeStoredFunc "*" = SF2 (*) "*"
makeStoredFunc "<" = SF2 (boolFToInteger (<)) "<"
makeStoredFunc "==" = SF2 (boolFToInteger (==)) "=="
makeStoredFunc "/=0" = SF1 (boolFToInteger (/=) 0) "/=0"
makeStoredFunc "==0" = SF1 (boolFToInteger (==) 0) "==0"
