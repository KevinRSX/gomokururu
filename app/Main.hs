module Main where

import Lib 
  (
    Piece (..), 
    genBoard,
    printBoard,
    placePiece
  )

main :: IO ()
main = do
    printBoard $ genBoard 10
    putStr "\n\n\n"
    printBoard $ placePiece (genBoard 10) White 0 0 