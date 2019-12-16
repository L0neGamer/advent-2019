# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html
ghc ./main.hs -i../ -prof -fprof-auto-top && ./main +RTS -p