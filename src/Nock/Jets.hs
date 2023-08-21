module Nock.Jets (add, dec) where

import Nock.Parser qualified
import Nock.Types

dec :: Noun
dec = Nock.Parser.noun "[8 [1 0] 8 [1 6 [5 [0 30] 4 0 6] [0 6] 9 2 10 [6 4 0 6] 0 1] 9 2 0 1]"

add :: Noun
add = Nock.Parser.noun "[ 6 [5 [1 0] 0 12] [0 13] 9 2 10 [6 [8 [9 21 0 7] 9 2 10 [6 0 28] 0 2] 4 0 13] 0 1 ]"
