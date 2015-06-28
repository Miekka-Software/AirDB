module Miekka.DB.Update where

import Miekka.DB.Struct
import Miekka.DB.Query

newDB :: [Value] -> DB
newDB vs = DBGroup (zip dbTags vs, [])

updateDB :: DB -> Tag -> Value -> DB
updateDB (DBGroup (tav, cont)) tag newVal = DBGroup (b++[(tag, newVal)]++a, cont)
  where tavFocus = (tag, tagValue tag tav)
        (b, a) = splitMat tavFocus tav

addTB :: DB -> [Value] -> DB
addTB (DBGroup (tav, cont)) vs = DBGroup (tav, newTB:cont)
  where newTB = DBGroup (zip tbTags vs, [])

deleteTB :: Selection -> DB
deleteTB (_, crumb:_) = DBGroup (tav, b++a)
  where (Crumb tav (b,a)) = crumb

updateTB :: Selection -> Tag -> Value -> DB
updateTB ((DBGroup (tav, cont)), crumb:_) tag newVal = DBGroup (dbtav, dbb++[newTB]++dba)
  where newTB = DBGroup (b++[(tag, newVal)]++a, cont)
        tavFocus = (tag, tagValue tag tav)
        (b, a) = splitMat tavFocus tav
        (Crumb dbtav (dbb,dba)) = crumb

addEN :: Selection -> [Tag] -> [Value] -> DB
addEN ((DBGroup (tav, cont)), crumb:_) tags vs = DBGroup (dbtav, b++[newTB]++a)
  where newEN = DBEntry $ zip tags vs
        newTB = DBGroup (tav, newEN:cont)
        (Crumb dbtav (b,a)) = crumb

deleteEN :: Selection -> DB
deleteEN (_, tbcmb:dbcmb:_) = DBGroup (dbtav, dbb++[newTB]++dba)
          where newTB = DBGroup (tbtav, tbb++tba)
                (Crumb tbtav (tbb,tba)) = tbcmb
                (Crumb dbtav (dbb,dba)) = dbcmb

updateEN :: Selection -> Tag -> Value -> DB
updateEN ((DBEntry tav), tbcmb:dbcmb:_) tag newVal = DBGroup (dbtav, dbb++[newTB]++dba)
  where newEN = DBEntry $ b++[(tag, newVal)]++a
        newTB = DBGroup (tbtav, tbb++[newEN]++tba)
        tavFocus = (tag, tagValue tag tav)
        (b, a) = splitMat tavFocus tav
        (Crumb tbtav (tbb,tba)) = tbcmb
        (Crumb dbtav (dbb,dba)) = dbcmb
