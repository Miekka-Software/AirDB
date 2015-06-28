module Miekka.DB.Query where

import Data.List
import Text.Regex.PCRE
import Miekka.DB.Struct

query :: Selection -> Tag -> Term -> [Selection]
query ((DBGroup (gtav, cont)), path) tag term = concatMap zipPath matches
  where zipPath m = zip [m] [(Crumb gtav (splitMat m cont):path)]
        matches = case cont of
          (DBGroup _:_) -> filter (\(DBGroup (tav,_)) -> (tagValue tag tav) =~ term) cont
          (DBEntry _:_) -> filter (\(DBEntry tav) -> (tagValue tag tav) =~ term) cont
          _           -> error $ "Failed to pattern match: " ++ show cont

tagValue :: Tag -> [TaV] -> Value
tagValue tag tav = snd . head $ filter (\(t,_) -> t == tag) tav

splitMat :: (Eq a) => a -> [a] -> ([a],[a])
splitMat elm lst = let (b, maa) = break (==elm) lst in (b, drop 1 maa)
