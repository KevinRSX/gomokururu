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
    putStr "\n"
    let board1 = placePiece (genBoard 10) White 4 5
    printBoard board1
    putStr "\n"
    let board2 = placePiece board1 Black 3 4
    printBoard board2

