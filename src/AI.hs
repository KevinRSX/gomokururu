module AI
  (
    getNextPos,
    buildTree,
    expandBoard
  )
where

import Game
import Data.Tree

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
expandBoard _  = [(7,8), (2,2)]