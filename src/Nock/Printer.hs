module Nock.Printer (pretty) where

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLB
import Nock

pretty :: RawNoun ann -> TL.Text
pretty noun = TLB.toLazyText $ pretty' noun False

pretty' :: RawNoun ann -> Bool -> TLB.Builder
pretty' (Atom a _) _ = TLB.decimal a
pretty' (Cell lhs rhs _) True = pretty' lhs False <> " " <> pretty' rhs True
pretty' (Cell lhs rhs _) False = "[" <> pretty' lhs False <> " " <> pretty' rhs True <> "]"
