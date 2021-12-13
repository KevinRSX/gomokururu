module Main where
import Text.Printf
import Lib


redANSI = "\ESC[31m"
greenANSI = "\ESC[32m"
defANSI = "\ESC[0m"
redify s = redANSI ++ s ++ defANSI
greenify s = greenANSI ++ s ++ defANSI

putCheckRes caseName eRes res = do
    putStrLn $ prettyCaseName ++ " " ++ passStr
    where passStr = if res == eRes then  greenify "passed" else redify "failed"
          prettyCaseName = printf "%-30s" caseName

main :: IO ()
main = do
    printBoard $ genBoard 10
    putStr "\n"
    let board1 = placePiece (genBoard 10) White 4 5
    printBoard board1
    putStr "\n"
    let board2 = placePiece board1 Black 3 4
    printBoard board2

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
    putStrLn "Done"