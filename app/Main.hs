module Main where

import Game 
import AI

main :: IO ()
main = do
    gameLoop (genBoard 10) Black 0 10

getPair :: IO (Int, Int)
getPair = do
    rl <- getLine
    cl <- getLine
    return (read rl, read cl)

takeTurn :: Board -> Piece -> Int -> IO Board
takeTurn board piece step = do
    putStrLn $ "Input " ++ (piece2emoji piece) ++ " row and col: "
    (brow, bcol) <- getPair
    if not (pieceValid board brow bcol)
        then do
            putStrLn "Invalid placement, try again."
            takeTurn board piece step
    else do
        let newBoard = placePiece board piece brow bcol
        showStepInfo Black step
        printBoard newBoard
        return newBoard

gameLoop :: Board -> Piece -> Int -> Int -> IO ()
gameLoop board piece step totalSteps = do
    putStrLn "\n====Current Board===="
    printBoard board
    putStrLn "====================="

    newBoard <- takeTurn board piece (step + 1)

    if step + 1 < totalSteps
        then gameLoop newBoard (reversePiece piece) (step + 1) totalSteps
    else return ()
