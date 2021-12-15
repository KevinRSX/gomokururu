module Main where
import Text.Printf
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

main :: IO ()
main = do
    -- Board placement
    putBoard $ genBoard 10
    putStr "\n"
    let board1 = placePiece (genBoard 10) White 4 5
    putBoard board1
    putStr "\n"
    let board2 = placePiece board1 Black 3 4
    putBoard board2

    -- Check win
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
    putStrLn "Done"
