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
--        contents <- Useful.readFile "inputryan.txt"
--        contents <- Useful.readFile "inputtest.txt"
--        contents <- Useful.readFile "inputtest2.txt"
--        contents <- Useful.readFile "inputtest3.txt"
--        contents <- Useful.readFile "inputtest4.txt"
--        contents <- Useful.readFile "inputtest5.txt"
        let rec = getRecipes contents
            ore = findResourcesForFuel 1 rec
        print $ ore
        print $ findFuelFor rec 1000000000000 1 1572360
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
        ingredients' = M.map (* multiplier) ingredients

neededIngredients' :: Ingredient -> Recipes -> StoredMaterials -> (StoredMaterials, Ingredients)
neededIngredients' (mat, 0) rec stm = (M.empty, M.empty)
neededIngredients' (mat, amount) rec stm
  | storedAmount >= amount = (M.insert mat (storedAmount - amount) stm, M.empty)
  | storedAmount <  amount = (M.insert mat (produced' + storedAmount - amount) stm, ingredients')
  where storedAmount = M.findWithDefault 0 mat stm
        (produced,  ingredients)  = rec M.! mat
        (produced', ingredients') = increaseTo (amount - storedAmount) produced ingredients

neededIngredients :: [Material] -> Ingredients -> Recipes -> StoredMaterials -> (StoredMaterials, Integer)
neededIngredients [] is _ stm = (stm, M.findWithDefault 0 "ORE" is)
neededIngredients (m:ms) is rec stm = neededIngredients noOre (combineIngredients (M.delete m is) is') rec stm'
  where (stm', is') = neededIngredients' (m, is M.! m) rec stm
        noOre = nub $ ms ++ (delete "ORE" $ M.keys is')

findResourcesForFuel :: Integer -> Recipes -> Integer
findResourcesForFuel i rec = snd $  neededIngredients ["FUEL"] (M.fromList [("FUEL", i)]) rec M.empty

findFuelFor' :: Recipes -> Integer -> Integer -> Integer -> Maybe Integer
findFuelFor' rec oreAim lower upper = binarySearch oreAim lower upper f
  where f = flip findResourcesForFuel rec

findFuelFor :: Recipes -> Integer -> Integer -> Integer -> Integer
findFuelFor rec oreAim lower upper = result $ findFuelFor' rec oreAim lower upper
  where result (Just a) = a
        result Nothing = findFuelFor rec oreAim upper (2 * upper)

combineIngredients :: Ingredients -> Ingredients -> Ingredients
combineIngredients i i' = M.unionWith (+) i i'

parseIngredient :: String -> Ingredient
parseIngredient str = (split!!1, read (split!!0))
  where split = splitOn " " str

getRecipes' :: [String] -> (Material, (Integer, Ingredients))
getRecipes' (x:[y]) = (mat, (amount, ingredients))
  where ingredients = M.fromList $ map parseIngredient (splitOn ", " x)
        (mat, amount) = parseIngredient y

getRecipes :: String -> Recipes
getRecipes str = M.fromList splitIntoRecipes
  where lines = fromStr str '\n'
        splitIntoRecipes = map (getRecipes'.(splitOn " => ")) lines
--        deeply_mapped = map ((map (map (splitOn " "))).(map (splitOn ", ")).(splitOn " => ")) $ splitOn "\n" contents
