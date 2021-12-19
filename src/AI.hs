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

computeScore :: Board -> Piece -> Int
computeScore _ _ = 1000000

-- Ref: 2019 project
buildTree :: Piece -> Board -> [(Int, Int)] -> Int -> Tree Board
buildTree piece board neighbors lvl = Node board $ children lvl neighbors
  where children _ []                  = []
        children 0 _                   = []
        children lvl ((row, col) : xs) =
          buildTree (reversePiece piece) newBoard newNeighbors (lvl - 1)
            : children lvl xs
            where newNeighbors = expandBoard $ newBoard
                  newBoard = placePiece board piece row col

maxAlpha :: Piece -> Int -> Int -> Int -> Tree Board -> Int
maxAlpha _ _ alpha _ (Node _ []) = alpha
maxAlpha piece lvl alpha beta (Node b (x:xs))
  | lvl == 0 = curScore
  | canFinish curScore = curScore
  | newAlpha >= beta = beta
  | otherwise = maxAlpha piece lvl newAlpha beta (Node b xs)
  where
    curScore = computeScore b piece
    canFinish score = score > 100000 || score < (-100000)
    newAlpha = max alpha $ minBeta piece (lvl - 1) alpha beta x

minBeta :: Piece -> Int -> Int -> Int -> Tree Board -> Int
minBeta _ _ _ beta (Node _ []) = beta
minBeta piece lvl alpha beta (Node b (x:xs))
  | lvl == 0 = curScore
  | canFinish curScore = curScore
  | alpha >= newBeta = alpha
  | otherwise = minBeta piece lvl alpha newBeta (Node b xs)
  where
    curScore = computeScore b piece
    canFinish score = score > 100000 || score < (-100000)
    newBeta = min beta $ maxAlpha piece (lvl - 1) alpha beta x

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

