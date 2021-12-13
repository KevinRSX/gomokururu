module Main where

import Lib
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V


main :: IO ()
-- main = do printBoard $ genBoard 17
main = do
    mutable <- M.replicate 10 White
    mm <- M.replicate 10 mutable
    v <- V.unsafeFreeze mm
    putStrLn $ show $ V.toList v