module UI where

import Brick
import Brick.BChan
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Graphics.Vty (mkVty)
import Graphics.Vty.Attributes
import Graphics.Vty.Config
import Graphics.Vty.Input.Events
import Logic

data Name = CursorName deriving (Eq, Ord)

data Tick = Tick

app :: App WholeState Tick Name
-- data App s e n = App
--   { appDraw :: s -> [Widget n],
--     appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n),
--     appHandleEvent :: BrickEvent n e -> EventM n s (),
--     appStartEvent :: EventM n s (),
--     appAttrMap :: s -> AttrMap
--   }

app =
  App
    { appDraw = drawUI,
      appChooseCursor = \_ -> showCursorNamed CursorName,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap defAttr []
    }

-- -- Create a chessboard grid based on the game state
-- createBoard :: WholeState -> Widget n
-- createBoard gameState = vBox $ map createRow [1 .. 15]
--   where
--     createRow y = hBox $ map (createCell y) [1 .. 15]
--     createCell y x =
--       withBorderStyle unicode $
--         border $
--           str "    " -- Replace with actual game state representation

-- createBoard :: WholeBoard -> Widget Name
-- createBoard (WholeBoard _ board) = vBox $ map createRow board
--   where
--     createRow row = hBox $ map createCell row
--     createCell dotStat = str $ show dotStat

drawRow :: [BoardDotStat] -> String
drawRow row = concatMap (\dotStat -> show dotStat ++ " ") row

drawBoard :: WholeBoard -> String
drawBoard (WholeBoard _ board) = unlines $ interleaveEmptyLines (map drawRow board)
  where
    interleaveEmptyLines [] = []
    interleaveEmptyLines [x] = [x]
    interleaveEmptyLines (x : xs) = x : "" : interleaveEmptyLines xs
-- drawBoard (WholeBoard _ board) = unlines $ map drawRow board

drawRawRow::[BoardDotStat]->String
drawRawRow row = concatMap show row

drawRawBoard :: WholeBoard -> String
drawRawBoard (WholeBoard _ board) = show board

-- -- Draw the user interface
drawUI :: WholeState -> [Widget Name]
drawUI (WholeState wholeBoard _ (cursorY, cursorX) _ _) =
  [ showCursor
      CursorName
      (Brick.Types.Location (cursorX * 3, cursorY * 2))
      $ str
      $ drawBoard wholeBoard
  ]

-- Define the App structure
-- app :: App WholeBoard e n -- 'e' and 'n' should be replaced with actual types
-- app =
--   App
--     { appDraw = drawUI,
--       appChooseCursor = neverShowCursor,
--       --   appHandleEvent = handleEvent,
--       appStartEvent = return (),
--       appAttrMap = const $ attrMap defAttr []
--     }

-- Handle events (keyboard inputs, etc.)
-- handleEvent :: BrickEvent Name Tick -> EventM Name WholeState ()
-- handleEvent = undefined

-- handleEvent :: BrickEvent Name Tick -> WholeState -> EventM Name WholeState()
-- handleEvent g (AppEvent Tick) = moveCursor state DirLeft

handleEvent :: BrickEvent Name Tick -> EventM Name WholeState ()
handleEvent (AppEvent Tick) = do
  modify $ \state -> state
handleEvent (VtyEvent (EvKey key [])) = do
  modify $ \state -> case key of
    KUp -> moveCursor state DirUp
    KDown -> moveCursor state DirDown
    KLeft -> moveCursor state DirLeft
    KRight -> moveCursor state DirRight
    KEnter -> case placePawnAtCursor state of
      Right newState -> newState
      Left errMsg -> state
    _ -> state
handleEvent _ = return ()

-- handleEvent gameState event =
--   -- Event handling logic goes here
--   -- This is where you update your game state in response to user inputs
--   return gameState -- Return the updated game state

-- The main function to run the UI
runUI :: IO ()
runUI = do
  let initialState = initWholeState 10 'b'
  -- finalState <- defaultMain app initialState
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  eventChan <- newBChan 10
  forkIO $ forever $ do
    writeBChan eventChan Tick
    threadDelay 2000000

  finalState <-
    customMain
      initialVty
      buildVty
      (Just eventChan)
      app
      initialState
  return ()