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
    emptyValid,
    inBoundary,
    reversePiece,
    chkBoardWinning,
    getLineFrmBoard
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
emptyValid :: Board -> Int -> Int -> Bool
emptyValid board row col =
  inBoundary board row col && (getBoard board ! row ! col) == Empty
    where db = dim board

inBoundary :: Board -> Int -> Int -> Bool
inBoundary board row col =
  row >= 0 && col >= 0 && row < db && col < db
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

getColFrmBoard :: Board -> Int -> Int -> Int -> [Piece]
getColFrmBoard board col rf rt
  | col < 0    = error "bad col"
  | col >= bDim = error "bad col: larger than dimension"
  | otherwise  = helper b [rf..rt]
    where b = getBoard board
          bDim = dim board
          helper b []     = []
          helper b (r:rs) = (b ! r ! col) : helper b rs

getLineFrmBoard :: Board -> Int -> Int -> Int -> Int -> [Piece]
getLineFrmBoard board fr fc tr tc
  | fr<0 || fc<0 || tr<0 || tc <0                = error "some coordinates are zeroes"
  | fr>=bDim || fc>=bDim || tr>=bDim || tc>=bDim = error "some coordinates are beyond dimension"
  | fr == tr && fc == tc   = [b ! fr ! fc]  -- A dot
  | fr == tr               = hLine b fr fc tc
  | fc == tc               = vLine board fc fr tr
  | abs (fr-tr) == abs (fc-tc) = dLine b fr fc tr tc
  | otherwise      = 
    error $ show fr ++ " " ++ show fc ++ "    " ++ show tr ++" " ++ show tc ++ " genDlineFrmBoard error: coordinates are wrong"
    where b    = getBoard board
          bDim = dim board

          hLine b row colA colB =
             V.toList (V.slice colA (colB-colA+1) (b ! row))

          vLine = getColFrmBoard

          dLine b fr fc tr tc =
            dHelper b ftr ftc
              where
                ftr = if fr < tr then [fr..tr] else [fr,fr-1..tr]
                ftc = if fc < tc then [fc..tc] else [fc,fc-1..tc]
                dHelper b [] []         = []
                dHelper b _ []          = []
                dHelper b [] _          = []
                dHelper b (r:rs) (c:cs) = b ! r ! c : dHelper b rs cs

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

-- Something is wrong with this function
getDrc fr fc dr dc dim len
  | len<=0 || nr < 0 || nc < 0 || nr >= dim || nc >= dim = (fr,fc)
  | otherwise = getDrc nr nc dr dc dim ll
    where nr = fr+dr
          nc = fc+dc
          ll = len-1

-- TODO: create type of Vector (Vector a)
getStarLines :: Int -> Int -> Int -> Board -> [[Piece]]
getStarLines row col llen board =
  -- horizontal
  [
  -- v 1.0 - lengthy
  -- V.toList (V.slice (col-ltLength+1) ltLength (getBoard board ! row)),
  -- V.toList (V.slice col              rtLength (getBoard board ! row)),

  -- v 2.0 - concise
  V.toList (V.slice hi hn (b ! row)),
  -- verticals
  getColFrmBoard board col ra rb,
  -- diagonals

  getLineFrmBoard board tlr tlc brr brc,
  getLineFrmBoard board blr blc trr trc
  ]
  where b    = getBoard board
        bDim = dim board
        -- ltLength = if col-llen < 0 then col+1 else llen
        -- rtLength = if col+llen > bDim then bDim - col else llen
        hi = if col - llen < 0 then 0 else col - llen + 1
        hn = if hi + hLen > bDim then bDim - hi else hLen
        hLen = llen*2-1

        ra = if row - llen < 0 then 0 else row - llen + 1
        rb = if row + llen >= bDim then bDim - 1 else row + llen - 1
        
        (tlr, tlc) = getDrc row col (-1) (-1) bDim llen
        (brr, brc) = getDrc row col 1    1    bDim llen
        (blr, blc) = getDrc row col 1    (-1) bDim llen
        (trr, trc) = getDrc row col (-1) 1    bDim llen