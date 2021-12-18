module Main where
import Text.Printf
import Data.Tree

import Game
import AI


redANSI = "\ESC[31m"
greenANSI = "\ESC[32m"
defANSI = "\ESC[0m"
redify s = redANSI ++ s ++ defANSI
greenify s = greenANSI ++ s ++ defANSI

putCheckRes caseName eRes res = do
    putStrLn $ prettyCaseName ++ " " ++ passStr
    where passStr = if res == eRes then  greenify "passed" else redify "failed"
          prettyCaseName = printf "%-40s" caseName

showTree :: Tree Board -> IO ()
showTree (Node board children) = do
  putBoard board
  showTreeHelper children

showTreeHelper :: [Tree Board] -> IO ()
showTreeHelper [] = do return ()
showTreeHelper (c:cs) = do
  showTree c
  showTreeHelper cs


boardPlacementTest :: IO ()
boardPlacementTest = do
    putBoard $ genBoard 10
    putStr "\n"
    let board1 = placePiece (genBoard 10) White 4 5
    putBoard board1
    putStr "\n"
    let board2 = placePiece board1 Black 3 4
    putBoard board2


checkWinTest :: IO ()
checkWinTest = do
    putCheckRes
        "No one is winning (5 Empty): "
        Nothing
        (whoIsWinning5 $ replicate 4 Black ++ replicate 4 White ++ replicate 5 Empty)

    putCheckRes
        "Black is winning (5 Black): "
        (Just Black)
        (whoIsWinning5 $ replicate 4 White ++ [Empty, White] ++ replicate 5 Black)

    putCheckRes
        "White is winning (5 White): "
        (Just White)
        (whoIsWinning5 $ replicate 4 Black ++ replicate 5 White)
    
    let t = placePieceFrmTuplesF (genBoard 17) ["BKJ","WKJ","BLM","BBC", "WOM", "WON","WOO","WOP","WOQ"]
    putCheckRes
        "White won on row O in a board: "
        (Just White)
        (chkBoardWinning 14 14 t)

    let t = placePieceFrmTuplesF (genBoard 17) ["BAA", "BBB", "BCC", "BDD", "BEE", 
                                                "WAQ", "WBP", "WCO", "WDN", "WEM", 
                                                "WQA", "WPB", "WOC", "WND", "WME",
                                                "BQQ", "BPP", "BOO", "BNN", "BMM"]
    putCheckRes
        "Black wins (AA-EE): "
        (Just Black)
        (chkBoardWinning 0 0 t)
    
    putCheckRes
        "White wins (AQ-EM): "
        (Just White)
        (chkBoardWinning 1 15 t)

    putCheckRes
        "White wins (QA-ME): "
        (Just White)
        (chkBoardWinning 16 0 t)

buildTreeTest :: IO ()
buildTreeTest = do
    let t = placePieceFrmTuplesF (genBoard 15) []
    
    showTree $ buildTree Black t (expandBoard t) 2

main :: IO ()
main = do
    boardPlacementTest
    checkWinTest
    buildTreeTest
    putStrLn "Done"
