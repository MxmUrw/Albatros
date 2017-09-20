
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

hello :: IO ()
hello = someFunc

bla = 3 *
