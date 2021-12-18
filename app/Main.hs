module Main where

import Game
import AI
import Data.Char

main :: IO ()
main = do
    gameLoop (genBoard 15) Black 0 10

getPair :: IO (Int, Int)
getPair = do
    line <- getLine
    let (rl:cl:_) = line
    let rlRet = ord (toUpper rl) - ord 'A'
        clRet = ord (toUpper cl) - ord 'A'
    return (rlRet, clRet)


takeTurn :: Board -> Piece -> Int -> IO (Board, Int, Int)
takeTurn board piece step = do
    putStrLn $ "It's " ++ piece2emoji piece ++ " turn."
    putStrLn $ "Please place your piece (e.g. KF):"
    -- putStrLn $ "Input " ++ piece2emoji piece ++ " row and col (e.g. FK):"
    (row, col) <- getPair
    if not (pieceValid board row col)
        then do
            putStrLn "Invalid placement, try again."
            takeTurn board piece step
    else do
        let newBoard = placePiece board piece row col
        showStepInfo piece step
        putBoard newBoard
        return (newBoard, row, col)

gameLoop :: Board -> Piece -> Int -> Int -> IO ()
gameLoop board piece step totalSteps = do
    putStrLn "\n====Current Board===="
    putBoard board
    putStrLn "====================="

    (newBoard, row, col) <- takeTurn board piece (step + 1)

    case chkBoardWinning row col newBoard of
        Nothing -> gameLoop newBoard (reversePiece piece) (step + 1) totalSteps
        (Just piece) -> do putStrLn $ piece2emoji piece ++ " wins!\nGame ended."
