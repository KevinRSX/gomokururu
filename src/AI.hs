module AI
  (
    getNextPos,
    buildTree
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
buildTree :: Piece -> Board -> [(Int, Int)] -> Tree Board
buildTree piece board neighbors = Node board []

-- Get a list (or vector) of points created by the next move
expandBoard :: Board -> [(Int, Int)]
expandBoard _  = []