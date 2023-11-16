{-# LANGUAGE InstanceSigs #-}
module Logic(BoardDotStat,boardDotStatCons,
             WholeBoard,initWholeBoard) where 

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

data PlayerSide = BlackPlayer|WhitePlayer
    deriving (Show)

getCorrespondentPawn::PlayerSide->BoardDotStat
getCorrespondentPawn playerSide = case playerSide of 
    BlackPlayer->BlackPawn
    WhitePlayer->WhitePawn


data WinStat = IWin|EnemyWin|InGame
    deriving (Show)

data WholeState = WholeState WholeBoard PlayerSide (Int,Int) WinStat
    deriving (Show)

wholeStateCons::Int->PlayerSide->WholeState
wholeStateCons size playerSide = WholeState (initWholeBoard size) playerSide (0,0) InGame

data Direction = DirUp|DirDown|DirLeft|DirRight
    deriving (Show,Eq)

getDirVector::Direction->(Int,Int)
getDirVector direction = case direction of 
    DirUp->(-1,0)
    DirDown->(1,0)
    DirLeft->(0,-1)
    DirRight->(0,1)

moveCursor::WholeState->Direction->WholeState
moveCursor (WholeState wb@(WholeBoard size wholeBoard) pside (cursorY,cursorX) ws) dir= do
    let (dirY,dirX) = getDirVector(dir)
    let yNew = max 0 (min (size-1) (cursorY+dirY))
    let xNew = max 0 (min (size-1) (cursorX+dirX))
    (WholeState wb pside (yNew,xNew) ws)
     

