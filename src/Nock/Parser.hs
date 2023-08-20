{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Nock.Parser (noun) where

import Control.Applicative
import Data.Text.Lazy as TL
import Data.Void (Void)
import Nock.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

noun :: TL.Text -> Noun
noun txt = case Text.Megaparsec.parse (space *> parser <* space) "" . TL.filter (\c -> c /= '.') $ txt of
  Right non -> non
  Left err -> error $ errorBundlePretty err

type Parser = Parsec Void TL.Text

parser :: Parser Noun
parser = try atomParser <|> cellParser

atomParser :: Parser Noun
atomParser = atom <$> decimal

cellParser :: Parser Noun
cellParser = do
  char '['
  space
  nons <- sugarCell
  char ']'
  pure $ toCell nons

sugarCell :: Parser [Noun]
sugarCell = do
  try (liftA2 (:) (try (parser <* space)) sugarCell) <|> pure []

toCell :: [Noun] -> Noun
toCell = \case
  ~(a : rest) -> case rest of
    [b] -> cell a b
    _ -> cell a $ toCell rest
