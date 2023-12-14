{-# LANGUAGE LambdaCase #-}

module UI where

import Brick
import Brick.BChan
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Client
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty (mkVty)
import Graphics.Vty.Attributes
import Graphics.Vty.Config
import Graphics.Vty.Input.Events
import Logic
import System.IO

data Name = CursorName deriving (Eq, Ord)

data Tick = Tick

data UIState = UIState
  { wholeState :: WholeState,
    handle :: Handle
  }

app :: App UIState Tick Name
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

drawRawRow :: [BoardDotStat] -> String
drawRawRow row = concatMap show row

drawRawBoard :: WholeBoard -> String
drawRawBoard (WholeBoard _ board) = show board

-- -- Draw the user interface
drawUI :: UIState -> [Widget Name]
drawUI uiState =
  let (WholeState wholeBoard _ (cursorY, cursorX) _ _) = wholeState uiState
   in [ showCursor
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

moveUICursor :: UIState -> Logic.Direction -> UIState
moveUICursor uiState dir =
  let ws = wholeState uiState
      newWholeState = moveCursor ws dir
   in uiState {wholeState = newWholeState}

handleEvent :: BrickEvent Name Tick -> EventM Name UIState ()
handleEvent (AppEvent Tick) = do
  state <- get
  case winStatus (wholeState state) of
    IWin -> liftIO (putStrLn "You won the game!")
    EnemyWin -> liftIO (putStrLn "You lost the game.") 
    InGame -> continueGame state
  where
    winStatus (WholeState _ _ _ _ winStat) = winStat


-- handleEvent (AppEvent Tick) = do
--   state <- get
--   liftIO (updateEnemyPawn (handle state) (wholeState state)) >>= \case
--     Left _ -> put $ state
--     Right newState -> put (state {wholeState = newState})

handleEvent (VtyEvent (EvKey key [])) = do
  state <- get
  case key of
    KUp -> modify $ \state -> moveUICursor state DirUp
    KDown -> modify $ \state -> moveUICursor state DirDown
    KLeft -> modify $ \state -> moveUICursor state DirLeft
    KRight -> modify $ \state -> moveUICursor state DirRight
    KEnter -> handleEnter state
    _ -> return ()
handleEvent (VtyEvent (EvResize _ _)) = return ()

continueGame :: UIState -> EventM Name UIState ()
continueGame state = do
  result <- liftIO (updateEnemyPawnNonBlocking (handle state) (wholeState state))
  case result of
    Left _ -> put $ state
    Right newState -> put (state {wholeState = newState})


handleEnter :: UIState -> EventM Name UIState ()
handleEnter state = do
  let ws = wholeState state
      hd = handle state
  result <- liftIO $ iPlacePawnAtCursor hd ws
  case result of
    Right newState -> put $ state {wholeState = newState}
    Left _ -> return ()

-- The main function to run the UI
runUI :: Handle -> IO ()
runUI handle = do
  -- let initialState = initWholeState 10 'b'
  wholeState <- initBoard handle
  let uiState = UIState wholeState handle
  -- finalState <- defaultMain app initialState
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  eventChan <- newBChan 10
  forkIO $ forever $ do
    writeBChan eventChan Tick
    threadDelay 100000

  finalState <-
    customMain
      initialVty
      buildVty
      (Just eventChan)
      app
      uiState
  return ()
