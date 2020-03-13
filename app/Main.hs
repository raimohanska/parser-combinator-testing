module Main where

import Lib

main :: IO ()
main = readPackages >>= putStrLn . (\packages -> "Read " ++ (show $ length packages) ++ " packages")
