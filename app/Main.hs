module Main (main) where

import System.IO
import qualified Data.Text.IO as TIO

import Logic(initWholeBoard)

main::IO ()
main = do
    hSetEncoding stdout utf8
    putStrLn (show (initWholeBoard 4))