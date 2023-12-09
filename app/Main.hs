module Main where

import qualified Data.Text.IO as TIO
import Logic
  ( BoardDotStat,
    WholeBoard,
    boardDotStatCons,
    enemyPlacePawn,
    fetchWholeState,
    getDotStat,
    iPlacePawn,
    initWholeBoard,
    initWholeState,
    moveCursor,
    placePawnAtCursor,
  )
import System.IO
import UI

main :: IO ()
main = do
  hSetEncoding stdout utf8
  -- let whState = initWholeState 10 'b'
  -- let st1 = fetchWholeState (iPlacePawn whState (5,5))
  -- let st2 = fetchWholeState (enemyPlacePawn st1 (6,5))
  -- let st3 = fetchWholeState (iPlacePawn st2 (5,6))
  -- let st4 = fetchWholeState (enemyPlacePawn st3 (6,6))
  -- let st5 = fetchWholeState (iPlacePawn st4 (5,7))
  -- let st6 = fetchWholeState (enemyPlacePawn st5 (6,7))
  -- let st7 = fetchWholeState (iPlacePawn st6 (5,8))
  -- let st8 = fetchWholeState (enemyPlacePawn st7 (6,8))
  -- let st9 = fetchWholeState (iPlacePawn st8 (5,9))
  -- putStrLn (show st9)
  hSetEncoding stdout utf8
  runUI