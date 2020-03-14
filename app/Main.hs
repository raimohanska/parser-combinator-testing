module Main where

import PackagesParser

main :: IO ()
main = readPackagesFromFile "status.real.txt" >>= putStrLn . (\packages -> "Read " ++ (show $ length packages) ++ " packages")
