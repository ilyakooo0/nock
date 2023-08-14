module Main (main) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char qualified as Char
import Data.Function
import Data.Text as T
import Data.Text.Encoding qualified as T
import Data.Text.IO as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Data.Text.Lazy.IO as TL
import Debug.Trace
import Debug.Trace (traceShowId)
import GHC.Base (unsafeChr)
import Nock
import Nock.Jam
import Nock.Parser
import Nock.Parser qualified
import Nock.Printer
import System.FilePath.Posix
import System.Process.Typed
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Hspec

main :: IO ()
main = do
  spec <- goldenTest
  defaultMain $ spec

goldenTest :: IO TestTree
goldenTest = do
  files <- findByExtension [".hoon"] "tests/golden"
  fmap (testGroup "nock") $ forM files $ \source -> do
    let target = replaceExtension source ".nock"
    pure $ goldenVsString (takeBaseName source) target $ do
      hoon <- BSL.readFile source
      nock <-
        fmap (Nock.Parser.noun . T.decodeUtf8 . BS.toStrict . BSL.dropEnd 6 . BSL.drop 11 . snd)
          . readProcessStdout
          $ proc "urbit" ["eval"]
            & setStdin (byteStringInput hoon)
            & setStderr nullStream
      let result = TL.encodeUtf8 . Nock.Printer.pretty . tar $ cell (atom 0) nock
      pure result
