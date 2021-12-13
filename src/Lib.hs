module Lib 
  ( 
    Piece (..),
    genBoard,
    printBoard
  )
where

import Data.Vector as V (Vector, map, replicate, toList)

type Board = Vector (Vector (Maybe Piece))
data Piece = White | Black | Empty
instance Show Piece where
  show White = "O"
  show Black = "X"
  show Empty = "-"

printBoard b = do
  mapM_ putStrLn (V.toList $ V.map getVPieceString b)

piece2emoji :: Maybe Piece -> [Char]
piece2emoji (Just White) = "⚪"
piece2emoji (Just Black) = "⚫"
piece2emoji Nothing = "🔹"

getVPieceString :: Vector (Maybe Piece) -> [Char]
getVPieceString vp = concatMap piece2emoji (V.toList vp)

genBoard :: Int -> Board
genBoard dim = V.replicate dim row
  where
    row = V.replicate dim Nothing