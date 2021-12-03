module Lib
    ( someFunc
    , Piece (..)
    ) where

import Data.Vector

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Board = Vector (Vector (Maybe Piece))

data Piece = White | Black

instance Show Piece where
  show White = "o"
  show Black = "x"