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
data Param = Rel Int | Abs Int
data Op = Input Int -- writes to this Int
        | Output Param
        | Mul Param Param Int
        | Add Param Param Int

main = do
        contents <- readFile "input.txt"
        print "arg"
        let testMap = fromStr contents
--            val' = execute 0 (Just (Map.insert 2 21 (Map.insert 1 76 testMap)))
--            val = iter (map (\a -> (execute 0 (Just a))) (allPerms testMap)) 0 0
--        print val
--        print val'

fromStr :: String -> [Int]
fromStr str = map (read) stringLst
  where stringLst = wordsWhen (==',') str

--parseList :: [Int] -> Map Int Op
--parseList (x:xs)
--  |

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
