{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
import Useful
import IntCode
import Data.String
import Data.List
import System.CPUTime
import Debug.Trace
import qualified Data.Map as M

data Object = Empty | Wall | Block | Paddle | Ball deriving (Show, Eq, Ord)
type Board = M.Map Point Object
type Score = Integer
type BallPos = Point
type PaddlePos = Point
type Blocks = [Point]
data BoardInfo = BoardInfo
      { board::Board
      , score::Score
      , ballPos::BallPos
      , paddlePos::PaddlePos
      , blocks::Blocks
      } deriving Show

main = do
        contents <- readFile "input.txt"
        let rn = initState contents []
            BoardInfo{..} = parseOut (out (runProg rn))
--        print outcome
        putStrLn $ drawBoard board
        print $ length $ filter (==Block) (M.elems board)
        print $ playGame rn
        putStr ""

playGame' :: ProgramState -> BoardInfo
playGame' !ps@ProgStat{es=Halted} = parseOut (out ps)
playGame' !ps@ProgStat{es=AwaitInput} = playGame' ps'
  where BoardInfo{..} = parseOut (out ps)
        adjustment | fst paddlePos > fst ballPos = -1
                   | fst paddlePos < fst ballPos = 1
                   | otherwise = 0
        ps' = runProg $ setEndState (setInput (clearBuffs ps) [adjustment]) Running

playGame :: ProgramState -> BoardInfo
playGame ps@(ProgStat {mem=mem}) = playGame' ps'
  where initialised = M.insert 0 2 mem
        ps' = runProg $ consPSFromMem initialised

drawObject :: Object -> Char
drawObject Empty = ' '
drawObject Wall = head "\x2588"
drawObject Block = '#'
drawObject Paddle = '-'
drawObject Ball = 'o'

drawBoard :: Board -> String
drawBoard board = concat [ getRow y | y <- [min_y..max_y]]
  where keys = M.keys board :: [Point]
        tup_cmp fnc a b = compare (fnc a) (fnc b)
        getMostVal cmp fnc ks = fnc $ cmp (tup_cmp fnc) ks
        max_x = getMostVal maximumBy fst keys
        min_x = 0
        max_y = getMostVal maximumBy snd keys
        min_y = 0
        getRow y = [drawObject (M.findWithDefault Empty (x,y) board) | x <- [min_x..max_x]] ++ "\n" :: String

toObject :: Integer -> Object
toObject 0 = Empty
toObject 1 = Wall
toObject 2 = Block
toObject 3 = Paddle
toObject 4 = Ball

parseOut :: [Integer] -> BoardInfo
parseOut [] = BoardInfo M.empty (-1) (-1,-1) (-1,-1) []
parseOut (t:0:(-1):xs) = BoardInfo board t ballPos paddlePos blocks
  where BoardInfo{..} = parseOut xs
parseOut (2:y:x:xs) = BoardInfo (M.insert pos Block board) score ballPos paddlePos (pos:blocks)
  where BoardInfo{..} = parseOut xs
        pos = (x,y)
parseOut (3:y:x:xs) = BoardInfo (M.insert pos Paddle board) score ballPos pos blocks
  where BoardInfo{..} = parseOut xs
        pos = (x,y)
parseOut (4:y:x:xs) = BoardInfo (M.insert pos Ball board) score pos paddlePos blocks
  where BoardInfo{..} = parseOut xs
        pos = (x,y)
parseOut (t:y:x:xs) = BoardInfo (M.insert pos (toObject t) board) score ballPos paddlePos blocks
  where BoardInfo{..} = parseOut xs
        pos = (x,y)

notEmpty :: [[a]] -> [[a]]
notEmpty [] = []
notEmpty ([]:xs) = notEmpty xs
notEmpty (x:xs) = x:notEmpty xs

compInt :: Integer -> Integer -> Integer
compInt x x'
  | x > x' = -1
  | x < x' = 1
  | x == x' = 0

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
