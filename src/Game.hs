module Game
  ( 
    Piece (..),
    Board (..),
    genBoard,
    printBoard,
    placePiece,
    whoIsWinning5,
    piece2emoji
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
type Board = Vector (Vector Piece)
data Piece = White | Black | Empty deriving (Eq)
instance Show Piece where
  show White = "W"
  show Black = "B"
  show _ = "_"

printBoard b = do
  mapM_ putStrLn (V.toList $ V.map getVPieceString b)

piece2emoji :: Piece -> [Char]
piece2emoji White = "⚪"
piece2emoji Black = "⚫"
piece2emoji Empty = "🔹"

getVPieceString :: Vector Piece -> [Char]
getVPieceString vp = concatMap piece2emoji (V.toList vp)

genBoard :: Int -> Board
genBoard dim = V.replicate dim row
  where
    row = V.replicate dim Empty


-- Modifying Board state
-- TODO (Kevin): Complete gameLoop
placePiece :: Board -> Piece -> Int -> Int -> Board
placePiece b p row col = b // [(row, updatedRow)]
  where updatedRow = (b ! row) // [(col, p)]


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