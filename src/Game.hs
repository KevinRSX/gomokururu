module Game
  ( 
    Piece (..),
    Board (..),
    genBoard,
    printBoard,
    placePiece,
    whoIsWinning5,
    piece2emoji,
    showStepInfo,
    pieceValid,
    reversePiece
  )
where

import Data.Vector as V
  (
    Vector,
    map,
    replicate,
    toList,
    fromList,
    (//),
    (!)
  )

import Data.List (group)

-- Algebraic data types for Board and Piece
-- TODO (Andreas): Use another branch to try 1D once the first task is done
data Board = Board { dim :: Int
                   , getBoard :: Vector (Vector Piece) }
 -- type Board = Vector (Vector Piece)
data Piece = White | Black | Empty deriving (Eq)

instance Show Piece where
  show White = "W"
  show Black = "B"
  show _ = "_"

reversePiece :: Piece -> Piece
reversePiece White = Black
reversePiece Black = White
reversePiece _ = error "invalid argument"

printBoard :: Board -> IO ()
printBoard b = do
  mapM_ putStrLn (V.toList $ V.map getVPieceString $ getBoard b)

piece2emoji :: Piece -> [Char]
piece2emoji White = "⚪"
piece2emoji Black = "⚫"
piece2emoji Empty = "🔹"

getVPieceString :: Vector Piece -> [Char]
getVPieceString vp = concatMap piece2emoji (V.toList vp)

genBoard :: Int -> Board
genBoard dim = Board dim bd
  where bd = V.replicate dim (V.replicate dim Empty)

showStepInfo :: Piece -> Int -> IO ()
showStepInfo p step = do
    putStrLn $ "\nStep " ++ (show step) ++ ": " ++ (piece2emoji p) ++ "'s move"


-- Modifying Board state
-- TODO (Kevin): Complete gameLoop
pieceValid :: Board -> Int -> Int -> Bool
pieceValid board row col = 
  row >= 0 && col >= 0 && row < db && col < db && (getBoard board ! row ! col) == Empty
    where db = dim board

placePiece :: Board -> Piece -> Int -> Int -> Board
placePiece board p row col = Board (dim board) bd
  where bd = b // [(row, updatedRow)]
        updatedRow = (b ! row) // [(col, p)]
        b = getBoard board


-- Check win
-- TODO (Kevin): Generic check win function
whoIsWinning :: [Piece] -> Int -> Maybe Piece
whoIsWinning line cLen = helper (group line) cLen
  where
    helper :: [[Piece]] -> Int -> Maybe Piece
    helper [] cLen = Nothing
    helper (x:xs) cLen
      | length x >= cLen && head x /= Empty = Just $ head x
      | otherwise = helper xs cLen

whoIsWinning5 :: [Piece] -> Maybe Piece
whoIsWinning5 line = whoIsWinning line 5