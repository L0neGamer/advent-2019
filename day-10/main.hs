import System.IO
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import System.CPUTime
import Data.Fixed (mod')

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

xor True False = True
xor False True = True
xor _ _ = False

isNeg :: (Num a, Ord a) => a -> Bool
isNeg x = x < 0

type Coord = (Int, Int)
data MyFrac = MFrac Int Int deriving (Show)

instance Eq MyFrac where
  (MFrac x y) == (MFrac x' y') = x==x' && y==y'

instance Ord MyFrac where
  m@(MFrac x y) `compare` m'@(MFrac x' y')
   | m == m' = EQ
   | y == y' = x `compare` x'
   | otherwise = y `compare` y'

main = do
        contents <- readFile "input.txt"
        let asteroids = getAsteroids contents
            anglesFromStart = map (angleFrom (head asteroids)) (tail asteroids)
            visibleFrom = map (\a -> (length (getVisible a asteroids), getVisible a asteroids, a)) asteroids
            center@(t,as',c) = last $ sort visibleFrom
        start <- getCPUTime
        print center
        print $ (cherryPick $ findVaporised c (closestTo c asteroids))!!199
        middle <- timeDif start
        end <- timeDif middle
        putStr ""

replaceAt n xs x = fst splitLst ++ [x] ++ (tail $ snd splitLst)
  where splitLst = splitAt (fromInteger n) xs

consMyFrac :: Int -> Int -> MyFrac
consMyFrac x y = MFrac (div x gcd'') (div y gcd'')
  where gcd' = gcd x y
        gcd'' | gcd' == 0 = 1
              | otherwise = gcd'

angleFrom :: Coord -> Coord -> MyFrac
angleFrom (x, y) (x', y') = consMyFrac x'' y''
  where x'' = x' - x
        y'' = y' - y

manhattan :: Coord -> Coord -> Int
manhattan (x,y) (x',y') = (abs (x - x')) + (abs (y - y'))

findAsteroids :: String -> Int -> Int -> [Coord]
findAsteroids [] x y = []
findAsteroids (s:ss) x y
  | s == '#' = (x,y) : findAsteroids ss (x+1) y
  | otherwise = findAsteroids ss (x+1) y

getAsteroids :: String -> [Coord]
getAsteroids str = asteroids
  where rows = fromStr str '\n'
        asteroids = concat [findAsteroids (rows!!y) 0 y | y <- [0..((length rows) - 1)]]

getVisible :: Coord -> [Coord] -> S.Set Coord
getVisible o as = fromMapToSet $ getVisible' o (closestTo o as')
  where as' = delete o as

getVisible' :: Coord -> [Coord] -> M.Map MyFrac Coord
getVisible' o [] = M.empty
getVisible' o (a:as) = space
  where rest = getVisible' o as
        space = M.insert (angleFrom o a) a rest

closestTo :: Coord -> [Coord] -> [Coord]
closestTo o = sortBy (\a b -> (closestTo' a) `compare` (closestTo' b))
  where closestTo' = manhattan o

fromMapToSet :: Ord b => M.Map a b -> S.Set b
fromMapToSet m = S.fromList $ M.elems m

findVaporised :: Coord -> [Coord] -> [[Coord]]
findVaporised o as = map snd $ findVaporised'' $ M.toList $ findVaporised' o (closestTo o as')
  where as' = delete o as

findVaporised'' :: [(MyFrac, [Coord])] -> [(MyFrac, [Coord])]
findVaporised'' as = sortBy (\(m,_) (m',_) -> clockWiseOrder m m') as

cherryPick :: [[a]] -> [a]
cherryPick as = lst ++ (cherryPick'' as')
  where (lst, as') = cherryPick' as
        cherryPick'' [] = []
        cherryPick'' as'' = cherryPick as'

cherryPick' :: [[a]] -> ([a], [[a]])
cherryPick' [] = ([], [])
cherryPick' ([]:as) = cherryPick' as
cherryPick' ((x:xs):as) = (x:lhs, xs:rhs)
  where (lhs, rhs) = cherryPick' as

findVaporised' :: Coord -> [Coord] -> M.Map MyFrac [Coord]
findVaporised' o [] = M.empty
findVaporised' o (a:as) = M.insert myFrac (a:prev) next
  where next = findVaporised' o as
        myFrac = angleFrom o a
        prev = M.findWithDefault [] myFrac next

toAngle :: RealFloat a => MyFrac -> a
toAngle m@(MFrac x y) = mod' ((atan2 (y') x' + (pi/2)) + (2*pi)) (2 * pi)
  where x' = fromIntegral x
        y' = fromIntegral y

clockWiseOrder :: MyFrac -> MyFrac -> Ordering
clockWiseOrder m m' = toAngle m `compare` toAngle m'

