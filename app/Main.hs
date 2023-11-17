module Main (main) where

import System.IO
import qualified Data.Text.IO as TIO

import Logic(BoardDotStat,boardDotStatCons,initWholeBoard)

main::IO ()
main = do
    hSetEncoding stdout utf8
    
