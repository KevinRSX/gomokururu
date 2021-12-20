module Main where

import Game
import AI
import Data.Char

main :: IO ()
main = do
    let t = placePieceFrmTuplesF (genBoard 15) ["BHH"]
    gameLoop t White 0

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
    if not (emptyValid board row col)
        then do
            putStrLn "Invalid placement, try again."
            takeTurn board piece step
    else do
        let newBoard = placePiece board piece row col
        showStepInfo piece step
        putBoard newBoard
        return (newBoard, row, col)

takeTurnAI :: Board -> Piece -> Int -> IO (Board, Int, Int)
takeTurnAI board piece step = do
    putStrLn $ "It's " ++ piece2emoji piece ++ " (AI)'s turn."
    let (row, col) = getNextPos board piece
        newBoard = placePiece board piece row col
    showStepInfo piece step
    putBoard newBoard
    return (newBoard, row, col)

gameLoop :: Board -> Piece -> Int -> IO ()
gameLoop board piece step = do
    putStrLn "\n====Current Board===="
    putBoard board
    putStrLn "====================="

    (newBoard, row, col) <- case piece of
        Black -> takeTurnAI board piece (step + 1)
        White -> takeTurnAI board piece (step + 1)

    putStrLn $ "Score for step " ++ (show $ step + 1) ++ ": " ++ (show $ computeScore2 newBoard piece)

    case chkBoardWinning row col newBoard of
        Nothing -> gameLoop newBoard (reversePiece piece) (step + 1)
        (Just piece) -> do putStrLn $ piece2emoji piece ++ " wins!\nGame ended."
