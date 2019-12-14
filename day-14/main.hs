{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
import Useful
import IntCode
import Data.String
import Data.List
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

type Material = String
type Ingredient = (Material, Integer)
type Ingredients = M.Map Material Integer
type StoredMaterials = M.Map Material Integer
type Recipes = M.Map Material (Integer, Ingredients)

main = do
        contents <- Useful.readFile "input.txt"
--        contents <- Useful.readFile "inputtest.txt"
--        contents <- Useful.readFile "inputtest2.txt"
--        contents <- Useful.readFile "inputtest3.txt"
--        contents <- Useful.readFile "inputtest4.txt"
--        contents <- Useful.readFile "inputtest5.txt"
        let rec = getRecipes contents
            ore = callGetFuel rec M.empty
        print $ rec
        print $ ore
--        print $ maxFuelGivenOre 0 1000000000000 1000000000000 rec M.empty
        putStr ""

getAmount :: Material -> Recipes -> Integer
getAmount mat rec = i
  where (i,_) = rec M.! mat

addToStore :: Material -> Recipes -> StoredMaterials -> StoredMaterials
addToStore mat rec stm = M.insert mat (newAmount + prevAmount) stm
  where newAmount = getAmount mat rec
        prevAmount = M.findWithDefault 0 mat stm

increaseTo :: Integer -> Integer -> Ingredients -> (Integer, Ingredients)
increaseTo aim produced ingredients = (produced', ingredients')
  where multiplier = ceiling $ (fromIntegral aim) / (fromIntegral produced)
        produced' = multiplier * produced
        ingredients' = M.fromList $ map (\(mat, i) -> (mat, i * multiplier)) (M.toList ingredients)

neededIngredients' :: Ingredient -> Recipes -> StoredMaterials -> (StoredMaterials, Ingredients)
neededIngredients' (mat, amount) rec stm
  | storedAmount >= amount = (M.insert mat (storedAmount - amount) stm, M.empty)
  | storedAmount <  amount = (M.insert mat (produced' + storedAmount - amount) stm, ingredients')
  where storedAmount = M.findWithDefault 0 mat stm
        (produced, ingredients) = rec M.! mat
        (produced', ingredients') = increaseTo (amount - storedAmount) produced ingredients

neededIngredients :: [Material] -> Ingredients -> Recipes -> StoredMaterials -> (StoredMaterials, Integer)
neededIngredients ["ORE"] is _ stm = (stm, is M.! "ORE")
--neededIngredients ("ORE":ms) is rec stm = neededIngredients (collapseMaterials $ ms ++ ["ORE"]) is rec stm
neededIngredients (m:ms) is rec stm = neededIngredients (collapseMaterials (ms ++ M.keys is')) (combineIngredients (M.delete m is) is') rec stm'
  where (stm', is') = neededIngredients' (m, is M.! m) rec stm

callGetFuel :: Recipes -> StoredMaterials -> (StoredMaterials, Integer)
callGetFuel rec stm = neededIngredients ["FUEL"] (M.fromList [("FUEL", 1)]) rec stm



--maxFuelGivenOre :: Recipes -> Integer -> Integer -> StoredMaterials -> Integer -> (StoredMaterials, Integer)
--maxFuelGivenOre acc oreLeft oreBeginning rec stm
--  | checkIfZeroed stm && acc > 0 = acc' + maxFuelGivenOre 0 (ore'') oreBeginning rec M.empty
--  | oreLeft > ore' = trace (show oreLeft ++"ore,acc"++ show acc ++ "ore'" ++ show ore') $ maxFuelGivenOre (acc+1) (oreLeft - ore') oreBeginning rec stm'
--  | otherwise = acc
--  where ret@(stm', ore') = callGetFuel rec stm
--        ratio = div oreBeginning (oreBeginning - oreLeft)
--        acc' = (acc) * ratio
--        ore'' = (oreBeginning - oreLeft)
--              | otherwise = oreLeft - ore'
--  | acc > 0 && checkIfZeroed stm = maxFuelGivenOre rec (primeOre - ore) primeOre stm' (acc * (div primeOre (primeOre - ore)))
--  | ore > ore' = maxFuelGivenOre rec (ore - ore') primeOre stm' (acc + 1)
--  | otherwise = (ore, acc)
--  where ret@(stm', ore') = callGetFuel rec (trace (show ore) stm)


checkIfZeroed :: StoredMaterials -> Bool
checkIfZeroed stm = 0 == sum'
  where sum' = (sum.(map snd)) (M.toList stm)

combineIngredients :: Ingredients -> Ingredients -> Ingredients
combineIngredients i i' = M.unionWith (+) i i'

removeDuplicates' :: (Eq a) => a -> [a] -> [a]
removeDuplicates' x' [] = []
removeDuplicates' x' (y:ys)
  | x' == y = removeDuplicates' x' ys
  | otherwise = y: removeDuplicates' x' ys

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates next
  where next = removeDuplicates' x xs

collapseMaterials :: [Material] -> [Material]
collapseMaterials materials = sortBy oreAtBack $ removeDuplicates materials

oreAtBack :: Material -> Material -> Ordering
oreAtBack m m'
  | m == m' = EQ
  | m == "ORE" = GT
  | m' == "ORE" = LT
  | otherwise = EQ

parseIngredient :: String -> Ingredient
parseIngredient str = (split!!1, read (split!!0))
  where split = fromStr str ' '

getRecipes' :: [String] -> (Material, (Integer, Ingredients))
getRecipes' (x:[y]) = (mat, (amount, ingredients))
  where ingredients = M.fromList $ map parseIngredient (fromStr x ',')
        (mat, amount) = parseIngredient y

getRecipes :: String -> Recipes
getRecipes str = M.fromList splitIntoRecipes
  where lines = fromStr str '\n'
        splitIntoRecipes = map (getRecipes'.(flip fromStr '=').(delete '>')) lines
