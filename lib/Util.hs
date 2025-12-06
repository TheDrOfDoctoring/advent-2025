
module Util where

import Data.Int

type Long = Int64

parsePair :: [a] -> (a, a)
parsePair (a:as) = (a, last as)

