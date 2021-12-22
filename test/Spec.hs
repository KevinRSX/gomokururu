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

putCheckRes :: (PrintfArg p, Eq a) => p -> a -> a -> IO Bool
putCheckRes caseName eRes res = do
    putStrLn $ prettyCaseName ++ " " ++ passStr
    return match
    where match = res == eRes
          passStr = if res == eRes then  greenify "passed" else redify "failed"
          prettyCaseName = printf "%-40s" caseName

showTree :: Tree Board -> IO ()
showTree (Node board children) = do
  putBoard board
  putStr "\n"
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


testWhoIsWinning :: IO Bool
testWhoIsWinning = do
    c1 <- putCheckRes
        "No one is winning (5 Empty): "
        Nothing
        (whoIsWinning5 $ replicate 4 Black ++ replicate 4 White ++ replicate 5 Empty)

    c2 <- putCheckRes
        "Black is winning (5 Black): "
        (Just Black)
        (whoIsWinning5 $ replicate 4 White ++ [Empty, White] ++ replicate 5 Black)

    c3 <- putCheckRes
        "White is winning (5 White): "
        (Just White)
        (whoIsWinning5 $ replicate 4 Black ++ replicate 5 White)

    return $ c1 && c2 && c3

testChkBoardWinning :: IO Bool
testChkBoardWinning = do
    let t = placePieceFrmTuplesF (genBoard 17) ["BKJ","WKJ","BLM","BBC", "WOM", "WON","WOO","WOP","WOQ"]

    c1 <- putCheckRes
        "White won on row O in a board: "
        (Just White)
        (chkBoardWinning 14 14 t)

    let t = placePieceFrmTuplesF (genBoard 17) ["BAA", "BBB", "BCC", "BDD", "BEE",
                                                "WAQ", "WBP", "WCO", "WDN", "WEM",
                                                "WQA", "WPB", "WOC", "WND", "WME",
                                                "BQQ", "BPP", "BOO", "BNN", "BMM"]

    -- let t = placePieceFrmTuplesF (genBoard 17) ["BAA"]

    c2 <- putCheckRes
        "Black wins (AA-EE): "
        (Just Black)
        (chkBoardWinning 0 0 t)

    c3 <- putCheckRes
        "White wins (AQ-EM): "
        (Just White)
        (chkBoardWinning 1 15 t)

    c4 <- putCheckRes
        "White wins (QA-ME): "
        (Just White)
        (chkBoardWinning 16 0 t)

    return $ c1 && c2 && c3 && c4

buildTreeTest :: IO ()
buildTreeTest = do
    putStrLn "Running buildTreeTest"
    let t = placePieceFrmTuplesF (genBoard 15) ["BGG"]
    showTree $ buildTree White t (expandBoard t) 2

main :: IO ()
main = do
    -- boardPlacementTest
    -- buildTreeTest

    putStrLn "> Running testcases..."

    let res = sequence [
                testWhoIsWinning,
                testChkBoardWinning
                ]
    allPassed <- and <$> res
    if allPassed then
        putStrLn "> Done"
    else
        error $ redify "Some test cases failed"

