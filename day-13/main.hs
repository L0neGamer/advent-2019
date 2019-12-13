{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
import Useful
import IntCode
import Data.String
import Data.List
import System.CPUTime
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

playGame' :: ProgramState -> Board -> BoardInfo
playGame' !ps@ProgStat{es=Halted} _ = parseOut (out ps)
playGame' !ps@ProgStat{es=AwaitInput} board' = playGame' ps' board''
  where BoardInfo{..} = parseOut (out ps)
        board'' = M.union board board'
        adjustment = compInt (fst paddlePos) (fst ballPos)
        ps' = runProg $ setEndState (setInput (clearBuffs ps) [adjustment]) Running

playGame :: ProgramState -> BoardInfo
playGame ps@(ProgStat {mem=mem}) = playGame' ps' M.empty
  where initialised = M.insert 0 2 mem
        ps' = runProg $ consPSFromMem initialised

drawObject :: Object -> Char
drawObject Empty = ' '
drawObject Wall = head "\x2588"
drawObject Block = '#'
drawObject Paddle = '-'
drawObject Ball = 'o'

drawBoard :: Board -> String
drawBoard board = "/" ++  bar ++ "\\\n" ++ (concat [ getRow y | y <- [min_y..max_y]]) ++ "\\" ++ bar ++ "/\n"
  where keys = M.keys board :: [Point]
        tup_cmp fnc a b = compare (fnc a) (fnc b)
        getMostVal cmp fnc ks = fnc $ cmp (tup_cmp fnc) ks
        max_x = getMostVal maximumBy fst keys
        min_x = 0
        max_y = getMostVal maximumBy snd keys
        min_y = 0
        bar = ['-' | _ <- [min_x..max_x]]
        getRow y = "|" ++ [drawObject (M.findWithDefault Empty (x,y) board) | x <- [min_x..max_x]] ++ "|\n" :: String

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
