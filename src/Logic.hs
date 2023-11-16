{-# LANGUAGE InstanceSigs #-}
module Logic (RetType,initWholeBoard) where 

data RetType t e= OK t|Fail e

data BoardDotStat = BlackPawn|WhitePawn|EmptyDot

instance Show BoardDotStat where
    show::BoardDotStat->String
    show BlackPawn = "⚫"
    show WhitePawn = "⚪"
    show EmptyDot = "🔶"

data WholeBoard = WholeBoard Int [[BoardDotStat]]

concatRowToStr::[BoardDotStat]->String
concatRowToStr row = foldl (\res bdstat->res++show bdstat) "" row

instance Show WholeBoard where
    show::WholeBoard->String
    show (WholeBoard size wholeBoard) = foldl (\res row->res++concatRowToStr row++"\n") "" wholeBoard

createRow::Int->[BoardDotStat]
createRow size = replicate size EmptyDot

initWholeBoard::Int->WholeBoard
initWholeBoard size = WholeBoard size (replicate size (createRow size))

-- >>> initWholeBoard 4
-- C:\Users\JIPING~1\AppData\Local\Temp\ext872A: withFile: invalid argument (cannot encode character '\128310')

getDotStat::WholeBoard->Int->Int->RetType BoardDotStat String
getDotStat (WholeBoard size wholeBoard) row col = do
    if row<0 || row>=size
        then Fail ("row invalid, expecting row between 0 and "++show size++" actually got "++show row)
    else if col<0 || col>=size
        then Fail ("col invalid, expecting col between 0 and "++show size++" actually got "++show col)
    else OK (wholeBoard!!row!!col)

-- setDotStat::WholeBoard->Int->Int->BoardDotStat->WholeBoard
-- setDotStat ()
