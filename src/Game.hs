module Game
  (
    Piece (..),
    Board (..),
    genBoard,
    putBoard,
    placePiece,
    placePieceFrmTuples,
    placePieceFrmTuplesF,
    whoIsWinning5,
    piece2emoji,
    showStepInfo,
    pieceValid,
    reversePiece,
    chkBoardWinning
  )
where

import Data.Vector as V
  (
    Vector,
    map,
    replicate,
    toList,
    fromList,
    slice,
    (//),
    (!)
  )

import Data.List (group)
import Data.Char
import Data.Maybe (mapMaybe)

-- Algebraic data types for Board and Piece
-- TODO (Andreas): Use another branch to try 1D once the first task is done
data Board = Board { dim :: Int
                   , getBoard :: Vector (Vector Piece) }
                   deriving (Show)
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

-- full-width alphabets and space are used to align with the emojis
-- if the board's dim goes beyond a specific number, then this becomes ugly
putBoard :: Board -> IO ()
putBoard b = do
  putStrLn $ '　' : take (length listifyStrBoard) ['Ａ'..]
  mapM_ (putStrLn . uncurry (:)) (zip ['Ａ'..] listifyStrBoard)
  where listifyStrBoard = V.toList $ V.map getVPieceString $ getBoard b

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
    putStrLn $ "\nStep " ++ show step ++ ": " ++ piece2emoji p ++ "'s move"


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

placePieceFrmTuples :: Board -> [(Piece, Int, Int)] -> Board
placePieceFrmTuples board [] = board
placePieceFrmTuples board (m:ms) =
  placePieceFrmTuples newBoard ms
    where newBoard = placePiece board p r c
          (p,r,c) = m

-- F stands for FANCY
placePieceFrmTuplesF :: Board -> [String] -> Board
placePieceFrmTuplesF board cList = placePieceFrmTuples board pList
  where pList = helper cList
        helper [] = []
        helper (x:xs) =
          (p,r,c) : helper xs
          where
            (cp:cr:cc:eol) = x
            p = if cp =='B' then Black else White
            r = ord cr - ord 'A'
            c = ord cc - ord 'A'

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

chkBoardWinning row col board
  | Black `elem` res && White `elem` res = error "Tie? IMPOSSIBLE!!!"
  | null res = Nothing
  | otherwise = Just $ head res  -- length res > 1 is possible : more than 5 pieces in a row
    where res = mapMaybe whoIsWinning5 sls
          sls = getStarLines row col 5 board

{-
Star lines
4   4   4
 3  3  3 
  2 2 2  
   111   
432101234
   111   
  2 2 2  
 3  3  3 
4   4   4

-}

-- TODO: create type of Vector (Vector a)
getStarLines :: Int -> Int -> Int -> Board -> [[Piece]]
getStarLines row col llen board =
  -- horizontal
  [
  -- v 1.0 - lengthy
  -- V.toList (V.slice (col-ltLength+1) ltLength (getBoard board ! row)),
  -- V.toList (V.slice col              rtLength (getBoard board ! row)),

  -- v 2.0 - concise
  V.toList (V.slice hi hn (getBoard board ! row))
  -- verticals
  -- diagonals
  ]
  where bDim = dim board
        -- ltLength = if col-llen < 0 then col+1 else llen
        -- rtLength = if col+llen > bDim then bDim - col else llen
        hi = if col - llen < 0 then 0 else col - llen + 1
        hn = if hi + hLen > bDim then bDim - hi else hLen
        hLen = llen*2-1
        