module AI
  (
    getNextPos,
    buildTree,
    expandBoard,
    computeScore,
    computeScore2,
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
import Control.Parallel.Strategies
import Control.DeepSeq

-- Constants
minInt :: Int
minInt = -(2 ^ 29)

maxInt :: Int
maxInt = 2 ^ 29 - 1

-- Tweakable parameters
treeLevel :: Int
treeLevel = 4 -- buildTree

searchLevel :: Int
searchLevel = 2 -- must be less than treeLevel

sequentialLevel :: Int
sequentialLevel = 0 -- level to be evaluated sequentially, must be
                    -- less than or equal to searchLevel

-- getNextPos: AI entry point
-- Assumes there is at least one piece on the board, otherwise buildTree will
-- return an empty tree
getNextPos :: Board -> Piece -> (Int, Int)
getNextPos board piece = boardDiff nextBoard board
  where
    (Node b children) = buildTree piece board neighbors treeLevel
    neighbors = expandBoard board
    minmax = parMap rdeepseq (minBeta piece searchLevel) children
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
        getDiff _ _ _ = error "unknown error"
        oldBoard1d = (V.toList . V.concat . V.toList) (getBoard oldBoard)
        newBoard1d = (V.toList . V.concat . V.toList) (getBoard newBoard)


get8NeighboursPoss dBoard r c = validNbPositions
  where validNbPositions = [
          (r + pr, c + pc) |
            pr <- [-1 .. 1],
            pc <- [-1 .. 1],
            (pr, pc) /= (0, 0),
            inBoundary dBoard (r+pr) (c+pc)
          ]

computeScore :: Board -> Piece -> Int
computeScore db p = csHelper 0 0 db p
  where
    csHelper :: Int -> Int -> Board -> Piece -> Int
    csHelper r c db p
      | c >= bdim = csHelper (r+1) 0 db p
      | r >= bdim || c >= bdim = 0
      | b ! r ! c /= p = next   -- ignore other piece / empty
      | otherwise = numOfGoodNb + next
        where bdim = dim db
              b    = getBoard db
              next = csHelper r (c + 1) db p
              numOfGoodNb = gnHelper b p nbs
                where nbs = get8NeighboursPoss db r c
                      gnHelper b p [] = 0
                      gnHelper b p (n:ns) =
                        fromEnum (b ! nr ! nc == p) + gnHelper b p ns
                        where (nr,nc) = n

computeScore2 :: Board -> Piece -> Int
computeScore2 board piece = runEval $ do
  ls2 <- rpar (force (map (lineScore2 piece) pieces))
  ls3 <- rpar (force (map (lineScore3 piece) pieces))
  ls4 <- rpar (force (map (lineScore4 piece) pieces))
  ls5 <- rpar (force (map (lineScore5 piece) pieces))
  return (sum ls2 + sum ls3 + sum ls4 + sum ls5)
  where
    pieces = getBoardLines board

lineScore2 :: Piece -> [Piece] -> Int
lineScore2 _ [] = 0
lineScore2 piece l
  | length l >= 4 = lineScore2Helper piece l
  | otherwise     = 0
      where
        lineScore2Helper piece (a:b:c:d:xs)
          | pieces4 == [Empty, piece, piece, Empty] = 10 + next
          | pieces4 == [reversePiece piece, piece, piece, Empty]
            || pieces4 == [Empty, piece, piece, reversePiece piece]
            = 5 + next
          | otherwise = next
          where pieces4 = [a, b, c, d]
                next    = lineScore2 piece (b:c:d:xs)
        lineScore2Helper _ _ = error "unknown error"

lineScore3 :: Piece -> [Piece] -> Int
lineScore3 _ [] = 0
lineScore3 piece l
  | length l >= 5 = lineScore3Helper piece l
  | otherwise     = 0
      where
        lineScore3Helper piece (a:b:c:d:e:xs)
          | pieces5 == [Empty, piece, piece, piece, Empty] = 100 + next
          | pieces5 == [reversePiece piece, piece, piece, piece, Empty]
            || pieces5 == [Empty, piece, piece, piece, reversePiece piece]
            = 50 + next
          | otherwise = next
          where pieces5 = [a, b, c, d, e]
                next = lineScore3 piece (b:c:d:e:xs)
        lineScore3Helper _ _ = error "unknown error"

lineScore4 :: Piece -> [Piece] -> Int
lineScore4 _ [] = 0
lineScore4 piece l
  | length l >= 6 = lineScore4Helper piece l
  | otherwise     = 0
      where
        lineScore4Helper piece (a:b:c:d:e:f:xs)
          | piece6 ==
              [Empty, piece, piece, piece, piece, Empty] = 1000 + next
          | piece6 == [reversePiece piece, piece, piece, piece, piece, Empty]
            || piece6 == [Empty, piece, piece, piece, piece, reversePiece piece]
            = 500 + next
          | otherwise = next
          where piece6 = [a,b,c,d,e,f]
                next = lineScore4 piece (b:c:d:e:f:xs)
        lineScore4Helper _ _ = error "unknown error"

lineScore5 :: Piece -> [Piece] -> Int
lineScore5 _ [] = 0
lineScore5 piece l
  | length l >= 5 = lineScore5Helper piece l
  | otherwise     = 0
      where
        lineScore5Helper piece (a:b:c:d:e:xs)
          | [a, b, c, d, e] ==
              [piece, piece, piece, piece, piece] = 10000 + lineScore5 piece (b:c:d:e:xs)
          | otherwise = lineScore5 piece (b:c:d:e:xs)
        lineScore5Helper _ _ = error "unknown error"

-- Ref: 2019 project
buildTree :: Piece -> Board -> [(Int, Int)] -> Int -> Tree Board
buildTree piece board neighbors lvl = Node board $ children lvl neighbors
  where children _ []                  = []
        children 0 _                   = []
        children lvl ((row, col) : xs) =
          buildTree (reversePiece piece) newBoard newNeighbors (lvl - 1)
            : children lvl xs
            where newNeighbors = expandBoard newBoard
                  newBoard = placePiece board piece row col

maxAlpha :: Piece -> Int -> Tree Board -> Int
maxAlpha piece lvl (Node b children)
  | lvl == 0 = curScore
  | curScore <= 1000 = curScore
  | lvl <= sequentialLevel = maximum $ parMap rdeepseq (minBeta piece (lvl - 1)) children
  | otherwise = maximum $ parMap rdeepseq (minBeta piece (lvl - 1)) children
  where
    curScore = computeScore2 b piece - computeScore2 b (reversePiece piece)

minBeta :: Piece -> Int -> Tree Board -> Int
minBeta piece lvl (Node b children)
  | lvl == 0 = curScore
  | curScore >= 1000 = curScore
  | lvl <= sequentialLevel = minimum $ map (maxAlpha piece (lvl - 1)) children
  | otherwise = minimum $ parMap rdeepseq (maxAlpha piece (lvl - 1)) children
  where
    curScore = computeScore2 b piece - computeScore2 b (reversePiece piece)

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
            emptyValid db (r+pr) (c+pc)
          ]

