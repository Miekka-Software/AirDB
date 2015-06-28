module Miekka.DB.Struct where

type Tag   = String
type Value = String
type Term  = String
type Title = String
type TaV   = (Tag,Value)
type Path  = [DBzip]
type Selection = (DB, Path)

data DB = DBGroup ([TaV], [DB]) | DBEntry [TaV]
  deriving (Eq,Read,Show)

data DBzip = Crumb [TaV] ([DB], [DB])
  deriving (Eq,Read,Show)

dbTags = ["dbTitle", "dbComment", "dbFilepath"]
tbTags = ["tbTitle", "tbComment"]

gContent :: DB -> [DB]
gContent (DBGroup (_, cont)) = cont

gTaV :: DB -> [TaV]
gTaV (DBGroup (tav, _)) = tav
