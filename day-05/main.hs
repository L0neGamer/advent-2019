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

--data ParamRel = Rel Int
--data ParamAbs = Abs Int
data Param = Rel | Abs deriving Show
data Op = Input -- Int -- writes to this Int
        | Output Param -- Param
        | Mul Param Param -- Param Param Int
        | Add Param Param -- Param Param Int
        | Halt
        | JmpNZ Param Param
        | JmpOZ Param Param
        | LessThan Param Param
        | EqualTo Param Param
        deriving Show
data Register = RegO Op
              | RegI Int deriving Show

main = do
        contents <- readFile "input.txt"
        print "arg"
        let testMap = fromInput contents
        print testMap
        let res = runProg testMap 0 []
        print res
--            val' = execute 0 (Just (Map.insert 2 21 (Map.insert 1 76 testMap)))
--            val = iter (map (\a -> (execute 0 (Just a))) (allPerms testMap)) 0 0
--        print val
--        print val'

--runProgram :: Map Int Int -> Map Int Int


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

runBinOp f p1 p2 i map = (Map.insert (getItem Abs (i+3) map) (f (getItem p1 (i+1) map) (getItem p2 (i+2) map)) map, i + 4, "", False)

runJmp f p1 p2 i map
  | f (getItem p1 (i+1) map) = (map, getItem p2 (i+2) map, "", False)
  | otherwise = (map, i+3, "", False)

boolFToInt f a b = fromEnum (f a b)

runOp :: Op -> Int -> Map Int Int -> (Map Int Int, Int, String, Bool)
runOp (Add p1 p2) i map = runBinOp (+) p1 p2 i map
runOp (Mul p1 p2) i map = runBinOp (*) p1 p2 i map
runOp (LessThan p1 p2) i map = runBinOp (boolFToInt (<)) p1 p2 i map
runOp (EqualTo p1 p2) i map = runBinOp (boolFToInt (==)) p1 p2 i map
runOp (JmpNZ p1 p2) i map = runJmp (/= 0) p1 p2 i map
runOp (JmpOZ p1 p2) i map = runJmp (== 0) p1 p2 i map
runOp (Halt) i map = (map, i + 1, "", True)
runOp (Output p1) i map = (map, i + 2, show $ getItem p1 (i+1) map, False)
runOp (Input) i map = (Map.insert (getItem Abs (i+1) map) (5) map, i + 2, "", False)

extractMaybe (Just x) = x
extractMaybe Nothing = error "tried to access Nothing"

getItem :: Param -> Int -> Map Int Int -> Int
getItem Abs i map = map Map.! i
getItem Rel i map = map Map.! (map Map.! i)

fromInput :: String -> Map Int Int
fromInput str = parseList (fromStr str)

fromStr :: String -> [Int]
fromStr str = map (read) stringLst
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
parseOp [x] = parseOp ['0',x]
parseOp [y,x] = constructOp (read [y,x]) Rel Rel
parseOp (x':y:[x]) = constructOp (read [y,x]) (getParamOp x') Rel
parseOp (x'':x':y:[x]) = constructOp (read [y,x]) (getParamOp x') (getParamOp x'')
parseOp xs = error $ "error on " ++ show xs

constructOp :: Int -> Param -> Param -> Op
constructOp 1 p1 p2 = Add p1 p2
constructOp 2 p1 p2 = Mul p1 p2
constructOp 3 _ _ = Input
constructOp 4 p1 _ = Output p1
constructOp 5 p1 p2 = JmpNZ p1 p2
constructOp 6 p1 p2 = JmpOZ p1 p2
constructOp 7 p1 p2 = LessThan p1 p2
constructOp 8 p1 p2 = EqualTo p1 p2
constructOp 99 _ _ = Halt
constructOp x _ _ = error $ "error on " ++ show x

--parseOp :: String -> [Int] -> ([Register], [Int])
--parseOp [x] ops = parseOp ['0',x] ops
--parseOp [y,x] ops = constructOp (read [y,x]) Rel Rel ops
--parseOp (x':y:[x]) ops = constructOp (read [y,x]) (getParamOp x') Rel ops
--parseOp (x'':x':y:[x]) ops = constructOp (read [y,x]) (getParamOp x') (getParamOp x'') ops
--parseOp xs ops = ([RegI (read xs)], ops)
--
--constructOp :: Int -> Param -> Param -> [Int] -> ([Register], [Int])
----constructOp 1 p1 p2 (x:x':x'':xs) = (RegO (Add p1 p2):RegI x:RegI x':RegI x'':[], xs)
--constructOp 2 p1 p2 (x:x':x'':xs) = (RegO (Mul p1 p2):RegI x:RegI x':RegI x'':[], xs)
--constructOp 3 _ _ (x:xs) = (RegO Input:RegI x:[], xs)
--constructOp 4 p1 _ (x:xs) = (RegO (Output p1):RegI x:[], xs)
--constructOp 99 _ _ xs = ([RegO Halt], xs)
--constructOp x _ _ xs = ([RegI x], xs) --error $ "error on " ++ show x ++ ", lst:" ++ show xs


--iter :: [Maybe (Map Int Int)] -> Int -> Int -> Maybe (Int, Int)
--iter [] noun verb = Nothing
--iter ((Just x):xs) noun verb
--  | booleanVal =  Just (verb, noun) -- these are named incorrectly
--  | otherwise = iter xs newNoun newVerb
--  where initVal = Map.lookup 0 x
--        booleanVal = case (initVal) of
--                        (Just 19690720) -> True
--                        (Just x) -> False
--                        Nothing -> False
--        newNoun = mod (noun + 1) 100
--        newVerb | newNoun < noun = verb + 1
--                | otherwise = verb

--allPerms :: Map Int Int -> [Map Int Int]
--allPerms myMap = allPerms' (allPerms' [myMap] 1 99) 2 99
--allPerms' :: [Map Int Int] -> Int -> Int -> [Map Int Int]
--allPerms' [] _ _ = []
--allPerms' (x:xs) index value = (map (\a -> Map.insert index a x) [0..value]) ++ (allPerms' xs index value)
--
--helper :: [a] -> [(Int, a)]
--helper a = helper' a 0
--
--helper' :: [a] -> Int -> [(Int, a)]
--helper' [] _ = []
--helper' (a:as) b = (b, a):  helper' as (b+1)
--
--
--quickOp :: (Int -> Int -> Int) -> Int -> Map Int Int -> Maybe (Map Int Int)
--quickOp f instr myMap =  execute (instr+4) $ res
--  where quickLookup a = Map.lookup a myMap
--        res = do
--                a <- quickLookup (instr+1) >>= quickLookup
--                b <- quickLookup (instr+2) >>= quickLookup
--                c <- quickLookup (instr+3)
--                return $ Map.insert (c) (f a b) myMap
--
--execute :: Int -> Maybe (Map Int Int) -> Maybe (Map Int Int)
--execute instr Nothing = Nothing
--execute instr (Just myMap) = res
--  where quickLookup a = Map.lookup a myMap
--        res = do
--                instrOp <- quickLookup instr
--                quickOp' instrOp instr myMap
--
--quickOp' :: Int -> Int -> Map Int Int -> Maybe (Map Int Int)
--quickOp' 1 a b = quickOp (+) a b
--quickOp' 2 a b = quickOp (*) a b
--quickOp' 99 _ myMap = Just myMap
--quickOp' _ _ _ = Nothing
--
--execute' :: String -> Maybe (Map Int Int)
--execute' str = execute 0 (Just $ fromStr str)
