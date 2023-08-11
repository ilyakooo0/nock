module Main (main) where

import Data.ByteString.Lazy qualified as BSL
import Data.Text as T
import Debug.Trace (traceShowId)
import Nock
import Nock.Jam
import Nock.Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  spec <- testSpec "nock" specs
  defaultMain spec

specs :: Spec
specs = describe "nock" $ do
  tarTest "[[[4 5] [6 14 15]] [0 7]]" "[14 15]"
  tarTest "[77 [2 [1 42] [1 1 153 218]]]" "[153 218]"
  tarTest "[[132 19] [4 0 3]]" "20"

  prop
    "rub (mat atom) = atom"
    ( \(NonNegative (a :: Integer)) (NonNegative (b :: Integer)) (NonNegative (c :: Integer)) ->
        withMaxSuccess 10000 $
          let atom = fromInteger (a * b * c * 11)
           in rub (mat atom) === atom
    )

tarTest :: Text -> Text -> Spec
tarTest source target =
  it (T.unpack ("*" <> source <> " = " <> target)) $
    tar (decode source) == decode target
