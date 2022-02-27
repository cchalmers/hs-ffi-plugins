module Test where

import Data.Word

myCoolerList :: [Word64]
myCoolerList = [5,6,7,8,9]

dupList :: [Word64] -> [Word64]
dupList l = l ++ l
