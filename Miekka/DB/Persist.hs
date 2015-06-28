module Miekka.DB.Persist where

import System.IO
import Miekka.DB.Query
import System.IO.Unsafe
import Miekka.DB.Struct

saveDB :: DB -> IO ()
saveDB db = writeFile filepath (show db)
  where filepath = tagValue "dbFilepath" $ gTaV db

--Put down your pitchforks! "unsafePerformIO" has no side effects here.
loadDB :: FilePath -> DB
loadDB filepath = read $ unsafePerformIO (readFile filepath)
