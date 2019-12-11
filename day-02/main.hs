import System.IO
import qualified Data.Map as M
import IntCode

main = do
        contents <- readFile "input.txt"
        let memInit = fromInput contents
            allMems = allPerms memInit
            val' = runProg (consPSFromMem (M.insert 2 2 (M.insert 1 12 memInit)))
            val = iter (map (\a-> mem $ runProg (consPSFromMem a)) allMems) 0 0
        print $ (mem val')M.! 0
        print $ (\(x,y) -> (100*x)+y) val

iter :: [M.Map Integer Integer] -> Integer -> Integer -> (Integer, Integer)
iter [] noun verb = (-1,-1)
iter (x:xs) noun verb
  | booleanVal = (verb, noun) -- these are named incorrectly
  | otherwise = iter xs newNoun newVerb
  where initVal = M.lookup 0 x
        booleanVal = case (initVal) of
                        (Just 19690720) -> True
                        (Just x) -> False
                        Nothing -> False
        newNoun = mod (noun + 1) 100
        newVerb | newNoun < noun = verb + 1
                | otherwise = verb
--
allPerms :: M.Map Integer Integer -> [M.Map Integer Integer]
allPerms myMap = allPerms' (allPerms' [myMap] 1 99) 2 99
allPerms' :: [M.Map Integer Integer] -> Integer -> Integer -> [M.Map Integer Integer]
allPerms' [] _ _ = []
allPerms' (x:xs) index value = (map (\a -> M.insert index a x) [0..value]) ++ (allPerms' xs index value)
