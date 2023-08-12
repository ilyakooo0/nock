module Main (main) where

import Data.ByteString.Lazy qualified as BSL
import Data.Text as T
import Data.Text.IO as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.IO as TL
import Debug.Trace
import Debug.Trace (traceShowId)
import Nock
import Nock.Jam
import Nock.Parser
import Nock.Parser qualified
import Nock.Printer
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.Hspec

main :: IO ()
main = do
  spec <- testSpec "nock" specs
  defaultMain $
    testGroup
      "nock"
      [ spec,
        goldenTest "add",
        goldenTest "dec"
        -- goldenVsString "add" "golden/add.nock" "golden/add.result.nock" $
        --   file <-
        --   TL.writeFile "golden/add.result.nock" $ Nock.Printer.pretty
      ]

goldenTest :: String -> TestTree
goldenTest name = do
  let source = "tests/golden/" <> name <> ".nock"
  let target = "tests/golden/" <> name <> ".result.nock"
  goldenVsFile name source target $ do
    print "before "
    inp <- T.readFile source
    TL.writeFile target (Nock.Printer.pretty . tar $ Nock.Parser.decode inp)

specs :: Spec
specs = describe "nock" $ do
  tarTest "[[[4 5] [6 14 15]] [0 7]]" "[14 15]"
  tarTest "[77 [2 [1 42] [1 1 153 218]]]" "[153 218]"
  tarTest "[[132 19] [4 0 3]]" "20"
  fasTest "[1 [531 25 99]]" "[531 25 99]"
  fasTest "[2 [531 25 99]]" "531"
  fasTest "[3 [531 25 99]]" "[25 99]"
  fasTest "[6 [531 25 99]]" "25"
  haxTest "[2 11 [22 33]]" "[11 33]"
  haxTest "[3 11 [22 33]]" "[22 11]"
  haxTest "[4 11 [[22 33] 44]]" "[[11 33] 44]"
  haxTest "[5 11 [[22 33] 44]]" "[[22 11] 44]"
  -- tarTest "[9 36 0 2.047]" "0"

  prop
    "rub (mat atom) = atom"
    ( \(NonNegative (a :: Integer)) (NonNegative (b :: Integer)) (NonNegative (c :: Integer)) ->
        withMaxSuccess 10000 $
          let atom = fromInteger (a * b * c * 11)
           in rub (mat atom) === atom
    )
  describe "cue (jam noun) = noun" $ do
    testJam "[77 [2 [1 42] [1 1 153 218]]]"

tarTest :: T.Text -> T.Text -> Spec
tarTest source target =
  it (T.unpack ("*" <> source <> " = " <> target)) $
    tar (decode source) == decode target

haxTest :: T.Text -> T.Text -> Spec
haxTest source target =
  it (T.unpack ("#" <> source <> " = " <> target)) $
    hax (decode source) == decode target

fasTest :: T.Text -> T.Text -> Spec
fasTest source target =
  it (T.unpack ("/" <> source <> " = " <> target)) $
    fas (decode source) == decode target

testJam :: T.Text -> Spec
testJam source =
  it ("cue (jam " ++ T.unpack source ++ ") = source") $
    cue (jam (decode source)) == decode source
