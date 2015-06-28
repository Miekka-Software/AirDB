{-|
TODO:
  1. Finish basic functionality in ideal conditions
  2. "Bullet Proof" the program so it throws meaningful exceptions
  3. Automate DB reconstruction from Crumbs
  4. Add unique id for every DB, TB, and EN.
  5. Optimize using ByteStrings and Maps
|-}

module Miekka.DB
( module Miekka.DB.Struct
, module Miekka.DB.Update
, module Miekka.DB.Query
, module Miekka.DB.Persist
) where

import Miekka.DB.Struct
import Miekka.DB.Update
import Miekka.DB.Query
import Miekka.DB.Persist
