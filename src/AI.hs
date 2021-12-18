module AI
  (
    getNextPos,
    buildTree,
    expandBoard
  )
where

import Game
import Data.Tree
import Data.Vector as V
  ( (!),
    (//),
  )

import Data.Set as S
  ( fromList,
    toList
  )

getNextPos :: Board -> Piece -> Int -> (Int, Int)
getNextPos _ Black step = (0, step `div` 2)
getNextPos _ White step = (9, 9 - (step `div` 2 - 1))

computeScore :: Board -> Int -> Int
computeScore _ x = x

-- Ref: 2019 project
buildTree :: Piece -> Board -> [(Int, Int)] -> Int -> Tree Board
buildTree piece board neighbors lvl = Node board $ children lvl neighbors
  where children _ []                  = []
        children 0 _                   = []
        children lvl ((row, col) : xs) =
          buildTree (reversePiece piece) (placePiece board piece row col) newNeighbors (lvl - 1) : children lvl xs
        newNeighbors = [(14, 14)]

-- Get a list (or vector) of points created by the next move
expandBoard :: Board -> [(Int, Int)]
expandBoard db = S.toList $ S.fromList $ ebHelper 0 0 db
  where
    ebHelper r c db
      | c >= bdim = ebHelper (r + 1) 0 db
      | r >= bdim || c >= bdim = []
      | b ! r ! c == Empty = next
      | otherwise = validNbPositions ++ next
      where
        b = getBoard db 
        bdim = dim db
        next = ebHelper r (c + 1) db
        validNbPositions = [
          (r + pr, c + pc) |
            pr <- [-1 .. 1],
            pc <- [-1 .. 1],
            (pr, pc) /= (0, 0),
            pieceValid db (r+pr) (c+pc)
          ]