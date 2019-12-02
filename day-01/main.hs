import System.IO
import Control.Monad

oneCalc :: Int -> Int
oneCalc a
  | curr > 0 = curr + oneCalc curr
  | otherwise = 0
    where curr = (-) (div a 3) 2

main = do
        contents <- readFile "input.txt"
        let words' = words contents
            list = f words' :: [Int]
        print $ show $ sum $ map oneCalc list

f = map read
