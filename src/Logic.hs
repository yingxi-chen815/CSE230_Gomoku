{-# LANGUAGE InstanceSigs #-}
module Logic(BoardDotStat,boardDotStatCons,
             WholeBoard(..),initWholeBoard,getDotStat,
             WholeState(..),initWholeState,Direction(..),
             moveCursor,iPlacePawn,placePawnAtCursor,enemyPlacePawn,
             fetchWholeState, concatRowToStr) where 

import Tools(set2DList)

data BoardDotStat = BlackPawn|WhitePawn|EmptyDot
    deriving (Eq)

boardDotStatCons::Char->BoardDotStat
boardDotStatCons 'b' = BlackPawn
boardDotStatCons 'w' = WhitePawn
boardDotStatCons _   = EmptyDot

instance Show BoardDotStat where
    show::BoardDotStat->String
    show BlackPawn = "⚫"
    show WhitePawn = "⚪"
    show EmptyDot = "🔶"

data WholeBoard = WholeBoard Int [[BoardDotStat]]

fromString::String->[BoardDotStat]
fromString str = map boardDotStatCons str

fromStrings::[String]->[[BoardDotStat]]
fromStrings strs = map fromString strs

-- >>> (WholeBoard 4 (fromStrings ["bbww","bbbw","..wb","..bw"]))
-- ⚫⚫⚪⚪
-- ⚫⚫⚫⚪
-- 🔶🔶⚪⚫
-- 🔶🔶⚫⚪

concatRowToStr::[BoardDotStat]->String
concatRowToStr row = foldl (\res bdstat->res++show bdstat) "" row

instance Show WholeBoard where
    show::WholeBoard->String
    show (WholeBoard size wholeBoard) = foldl (\res row->res++concatRowToStr row++"\n") "" wholeBoard

createRow::Int->[BoardDotStat]
createRow size = replicate size EmptyDot

initWholeBoard::Int->WholeBoard
initWholeBoard size = WholeBoard size (replicate size (createRow size))

-- >>> getDotStat (initWholeBoard 4) 2 1 
-- Right 🔶

getDotStat::WholeBoard->Int->Int->Either String BoardDotStat 
getDotStat (WholeBoard size wholeBoard) row col = do
    if row<0 || row>=size
        then Left ("row invalid, expecting row between 0 and "++show size++" actually got "++show row)
    else if col<0 || col>=size
        then Left ("col invalid, expecting col between 0 and "++show size++" actually got "++show col)
    else Right (wholeBoard!!row!!col)



setDotStat::WholeBoard->Int->Int->BoardDotStat->Either String WholeBoard
setDotStat (WholeBoard size wholeBoard) row col newState = do
    if row<0 || row>=size
        then Left ("row invalid, expecting row between 0 and "++show size++" actually got "++show row)
    else if col<0 || col>=size
        then Left ("col invalid, expecting col between 0 and "++show size++" actually got "++show col)
    else if (wholeBoard!!row!!col)==EmptyDot
        then Right (WholeBoard size (set2DList wholeBoard row col newState))
    else Left (show row++","++show col++" is already occupied")

--count how many connected black/white pawns there are from (y,x) towards (yDir,xDir) direction
countPawn::WholeBoard->BoardDotStat->(Int,Int)->(Int,Int)->Int
countPawn (WholeBoard size wholeBoard) bds (y,x) (yDir,xDir) = do
    if y<0||y>=size||x<0||x>=size
        then 0
    else if (wholeBoard!!y!!x)==bds
        then 1+(countPawn (WholeBoard size wholeBoard) bds (y+yDir,x+xDir) (yDir,xDir))
    else 0

countPawnBiDir::WholeBoard->BoardDotStat->(Int,Int)->(Int,Int)->Int
countPawnBiDir wb bds (y,x) (yDir,xDir) = (countPawn wb bds (y,x) (yDir,xDir))+(countPawn wb bds (y,x) (-yDir,-xDir))-1

-- >>> countPawnBiDir (WholeBoard 4 (fromStrings ["bbww",".bbw","..wb","..bw"])) BlackPawn (1,2) (0,1)
-- 2

--if there are 5 connected bds-pawns through one of 4 directions, somebody wins
isWin::WholeBoard->BoardDotStat->(Int,Int)->Bool
isWin wb bds (y,x) = foldl (\res dir->res||((countPawnBiDir wb bds (y,x) dir)>=5)) False [(1,0),(0,1),(1,1),(-1,1)]

data PlayerSide = BlackPlayer|WhitePlayer
    deriving (Eq,Show)

getCorrespondentPawn::PlayerSide->Bool->BoardDotStat
getCorrespondentPawn playerSide isMyTurn = case playerSide of 
    BlackPlayer->if isMyTurn then BlackPawn else WhitePawn
    WhitePlayer->if isMyTurn then WhitePawn else BlackPawn


data WinStat = IWin|EnemyWin|InGame
    deriving (Eq,Show)

data WholeState = WholeState WholeBoard PlayerSide (Int,Int) Bool WinStat
    deriving (Show)

initWholeState::Int->Char->WholeState
initWholeState size playerSideCh = WholeState (initWholeBoard size) playerSide (0,0) (playerSide==BlackPlayer) InGame
    where playerSide=if playerSideCh=='b' then BlackPlayer else WhitePlayer

data Direction = DirUp|DirDown|DirLeft|DirRight
    deriving (Show,Eq)

getDirVector::Direction->(Int,Int)
getDirVector direction = case direction of 
    DirUp->(-1,0)
    DirDown->(1,0)
    DirLeft->(0,-1)
    DirRight->(0,1)
     
placePawn::WholeState->(Int,Int)->Bool->Either String WholeState  --place a new pawn on the board, the 3rd argument indicated whether the player place his pawn, or enemy places his pawn
placePawn (WholeState wb@(WholeBoard size wholeBoard) pside (cursorY,cursorX) whoseTurn winS) (y,x) isMyPawn = do
    if winS==InGame then do    
        if whoseTurn==isMyPawn 
        then do
            let originPawn = getDotStat wb y x
            case originPawn of
                Left errorMsg->if isMyPawn then Left ("cannot place pawn here because "++errorMsg) else Left ("enemy cheating: "++errorMsg)
                Right bds->do
                    if not (bds==EmptyDot)
                        then Left("cannot place pawn at "++show y++","++show x++" because here is not empty")
                    else do
                        let pawnToPlace = getCorrespondentPawn pside isMyPawn
                        let newWholeBoard = setDotStat wb y x pawnToPlace 
                        case newWholeBoard of
                            Left errorMsg-> Left ("unexpected situation: "++errorMsg) -- this is never expected to happen when code runs
                            Right newWB -> do
                                let placerWin = isWin newWB pawnToPlace (y,x)
                                let newWS = if placerWin then (if isMyPawn then IWin else EnemyWin) else InGame
                                Right (WholeState newWB pside (cursorY,cursorX) (not whoseTurn) newWS)
        else
            Left ("It should be "++getPlayer whoseTurn++"'s turn, "++getPlayer isMyPawn++" cannot move now")
    else Left ("Game already ends with "++show winS)
    where getPlayer b= if b then "player" else "enemy" 

moveCursor::WholeState->Direction->WholeState
moveCursor (WholeState wb@(WholeBoard size wholeBoard) pside (cursorY,cursorX) whoseTurn ws) dir= do
    let (dirY,dirX) = getDirVector(dir)
    let yNew = max 0 (min (size-1) (cursorY+dirY))
    let xNew = max 0 (min (size-1) (cursorX+dirX))
    (WholeState wb pside (yNew,xNew) whoseTurn ws)

iPlacePawn::WholeState->(Int,Int)->Either String WholeState
iPlacePawn ws (y,x)=placePawn ws (y,x) True

placePawnAtCursor::WholeState->Either String WholeState
placePawnAtCursor ws@(WholeState (WholeBoard size wholeBoard) pside (cursorY,cursorX) whoseTurn winS)=placePawn ws (cursorY,cursorX) True

enemyPlacePawn::WholeState->(Int,Int)->Either String WholeState
enemyPlacePawn ws (y,x)=placePawn ws (y,x) False

fetchWholeState::Either String WholeState->WholeState
fetchWholeState (Left errMsg) = error errMsg
fetchWholeState (Right ws) = ws