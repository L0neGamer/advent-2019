import IntCode

main = do
        contents <- readFile "input.txt"
        print $ runStr contents [5]
