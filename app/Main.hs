module Main where

import Lib (printBoard, genBoard)

main :: IO ()
main = do printBoard $ genBoard 17