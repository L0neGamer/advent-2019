import System.IO
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

---- thanks to https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

data Param = Rel | Abs deriving Show
data Op = Input -- Int
        | Output Param
        | BinOp (Int -> Int -> Int) Param Param
--        | Mul Param Param -- Int
--        | Add Param Param -- Int
        | Halt
        | JmpNZ Param Param -- Int
        | JmpOZ Param Param -- Int
--        | LessThan Param Param -- Int
--        | EqualTo Param Param -- Int
--        deriving Show
data Register = RegO Op
              | RegI Int
--              deriving Show

main = do
        contents <- readFile "input.txt"
        let testMap = fromInput contents
        let res = runProg testMap 0 []
        print res

runStr :: String -> (Map Int Int, [String])
runStr str = runProg (fromInput str) 0 []

runProg :: Map Int Int -> Int -> [String] -> (Map Int Int, [String])
runProg map i out = next
  where op = parseOp $ (show $ map Map.! i)
        (map', i', outStr, halt) = runOp op i map
        strs | length outStr > 0 = out ++ [outStr]
             | otherwise = out
        next | not halt = runProg map' i' strs
             | otherwise = (map', strs)

runBinOp :: (Int -> Int -> Int) -> Param -> Param -> Int -> Map Int Int -> (Map Int Int, Int, String, Bool)
runBinOp f p1 p2 i map = (Map.insert (getItem Abs (i+3) map) (f (getItem p1 (i+1) map) (getItem p2 (i+2) map)) map, i + 4, "", False)

runJmp f p1 p2 i map
  | f (getItem p1 (i+1) map) = (map, getItem p2 (i+2) map, "", False)
  | otherwise = (map, i+3, "", False)

boolFToInt f a b = fromEnum (f a b)

runOp :: Op -> Int -> Map Int Int -> (Map Int Int, Int, String, Bool)
--runOp (Add p1 p2)       = runBinOp (+) p1 p2
--runOp (Mul p1 p2)       = runBinOp (*) p1 p2
--runOp (LessThan p1 p2)  = runBinOp (boolFToInt (<)) p1 p2
--runOp (EqualTo p1 p2)   = runBinOp (boolFToInt (==)) p1 p2
runOp (BinOp f p1 p2)   = runBinOp f p1 p2
runOp (JmpNZ p1 p2)     = runJmp (/= 0) p1 p2
runOp (JmpOZ p1 p2)     = runJmp (== 0) p1 p2
runOp (Halt)            = \i map -> (map, i + 1, "", True)
runOp (Output p1)       = \i map -> (map, i + 2, show $ getItem p1 (i+1) map, False)
runOp (Input)           = \i map -> (Map.insert (getItem Abs (i+1) map) (5) map, i + 2, "", False)

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
parseOp xs             = error $ "error on " ++ show xs

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
constructOp x  _ _  = error $ "error on " ++ show x
