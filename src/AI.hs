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
    concat,
    toList
  )

import Data.Set as S
  ( fromList,
    toList
  )

import Data.List (elemIndex)
import Data.Maybe (fromJust)

minInt :: Int
minInt = -(2 ^ 29)

maxInt :: Int
maxInt = 2 ^ 29 - 1

-- getNextPos: AI entry
-- Assumes there is at least one piece on the board, otherwise buildTree will
-- return an empty tree
getNextPos :: Board -> Piece -> (Int, Int)
getNextPos board piece = boardDiff nextBoard board
  where
    (Node b children) = buildTree piece board neighbors 5
    neighbors = expandBoard board
    minmax = map (minBeta piece 3 minInt maxInt) children
    index = fromJust $ elemIndex (maximum minmax) minmax
    (Node nextBoard _) = children !! index


-- This is shit code, kindly refrain from copying!
-- Assumptions:
-- 1. two boards have equal dim
-- 2. only differ in one bit
boardDiff :: Board -> Board -> (Int, Int)
boardDiff oldBoard newBoard = (drow, dcol) 
  where (drow, dcol) = quotRem diffPos (dim newBoard)
        diffPos = getDiff oldBoard1d newBoard1d 0
        getDiff [] [] _ = error "Invalid parameters"
        getDiff (x:xs) (y:ys) ind | x /= y = ind
                                  | x == y = getDiff xs ys (ind + 1)
        oldBoard1d = (V.toList . V.concat . V.toList) (getBoard oldBoard)
        newBoard1d = (V.toList . V.concat . V.toList) (getBoard newBoard)


get8NeighboursPoss dBoard r c = validNbPositions
  where validNbPositions = [
          (r + pr, c + pc) |
            pr <- [-1 .. 1],
            pc <- [-1 .. 1],
            (pr, pc) /= (0, 0),
            pieceValid dBoard (r+pr) (c+pc)
          ]

computeScore :: Board -> Piece -> Int
computeScore db p = csHelper 0 0 db p
  where
    csHelper :: Int -> Int -> Board -> Piece -> Int
    csHelper r c db p
      | c >= bdim = csHelper (r+1) 0 db p
      | r >= bdim || c >= bdim = 0
      | b ! r ! c /= p = next   -- ignore other piece / empty
      | otherwise = 0 -- numOfGoodNb + next
        where bdim = dim db
              b    = getBoard db
              next = csHelper r (c + 1) db p
              numOfGoodNb = gnHelper b p nbs
                where nbs = get8NeighboursPoss db r c
                      gnHelper b p [] = 0
                      gnHelper b p (n:ns) =
                        fromEnum (b ! nr ! nc == p) + gnHelper b p ns
                        where (nr,nc) = n



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
    canFinish score = score > 10 || score < (-10)
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
    canFinish score = score > 10 || score < (-10)
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

