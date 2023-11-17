{-# LANGUAGE InstanceSigs #-}
module Tools(set2DList) where

setList::[a]->Int->a->[a]
setList [] _ _ = error "Index out of range in setList"
setList (a:as) 0 aNew = (aNew:as)
setList (a:as) i aNew = (a:(setList as (i-1) aNew))

set2DList::[[a]]->Int->Int->a->[[a]]
set2DList [] _ _ _ = error "Index out of range in setList"
set2DList (row:rows) 0 x aNew = ((setList row x aNew):rows)
set2DList (row:rows) y x aNew = (row:(set2DList rows (y-1) x aNew))