module Useful where
import System.CPUTime
import qualified System.IO

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

readFile str = System.IO.readFile str

prime_factors :: Integer -> [Integer]
prime_factors 1 = []
prime_factors i = divisor:next
  where divisor = prime_factors' i primes
        next = prime_factors (div i divisor)
prime_factors' :: Integer -> [Integer] -> Integer
prime_factors' i (p:ps)
  | rem i p == 0 = p
  | otherwise = prime_factors' i ps

primes = 2 : primes'
  where isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
        primes' = 3 : filter (isPrime primes') [5, 7 ..]

notEmpty :: [[a]] -> [[a]]
notEmpty [] = []
notEmpty ([]:xs) = notEmpty xs
notEmpty (x:xs) = x:notEmpty xs

compInt :: Integer -> Integer -> Integer
compInt x x'
  | x > x' = -1
  | x < x' = 1
  | x == x' = 0

binarySearch :: (Integral a, Ord b) => b -> a -> a -> (a -> b) -> Maybe a
binarySearch aim lower upper f
  | aim > upperVal = Nothing
  | pivotVal == aim = Just pivot
  | upperVal == aim = Just upper
  | lowerVal == aim = Just lower
  | pivot == lower && upperVal > aim = Just pivot
  | aim > pivotVal = binarySearch aim pivot upper f
  | aim < pivotVal = binarySearch aim lower pivot f
  where lowerVal = f lower
        upperVal = f upper
        pivot = div (lower+upper) 2
        pivotVal = f pivot
