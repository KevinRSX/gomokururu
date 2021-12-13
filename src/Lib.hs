module Lib 
  ( 
    Piece (..),
    genBoard,
    printBoard,
    placePiece
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

type Board = Vector (Vector Piece)
data Piece = White | Black | Empty
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

placePiece :: Board -> Piece -> Int -> Int -> Board
placePiece b p row col = b // [(row, updatedRow)]
  where updatedRow = (b ! row) // [(col, p)]