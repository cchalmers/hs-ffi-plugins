module Plug2 (myCoolList) where
import Data.Word

data MyCoolData = MyCoolData {
  myCoolField :: Int,
  mySecondCoolField :: Word64
}

-- | A super cool list that will last for a very long time.
-- @
-- some code goes here
-- @
myCoolList :: [Word64]
myCoolList = [3,13..30]
