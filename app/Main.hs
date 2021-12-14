module Main where

import Game 
  (
    Board (..),
    Piece (..), 
    genBoard,
    printBoard,
    placePiece,
    piece2emoji
  )

main :: IO ()
main = do
    gameLoop (genBoard 10) 0

gameLoop :: Board -> Int -> IO ()
gameLoop board step = do
    let boardB = placePiece board Black 1 2
    showStepInfo Black (step + 1)
    printBoard boardB
    showStepInfo White (step + 2)
    let boardW = placePiece boardB White 3 4
    printBoard boardW
    if step + 2 < 2
        then gameLoop boardW (step + 2)
    else return ()

showStepInfo :: Piece -> Int -> IO ()
showStepInfo p step = do
    putStrLn $ "\n" ++ (piece2emoji p) ++ "'s Move: step " ++ (show step)