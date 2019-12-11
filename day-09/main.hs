import System.CPUTime
import IntCode
import Useful

main = do
        contents <- readFile "input.txt"
        start <- getCPUTime
        print $ runStr contents [2]
        middle <- timeDif start
        end <- timeDif middle
        putStr ""
