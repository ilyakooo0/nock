{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Nock.Parser (decode) where

import Data.Text as T
import Data.Void (Void)
import Nock
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

decode :: Text -> Noun
decode txt = case Text.Megaparsec.parse parser "" . T.filter (\c -> c /= '.') $ txt of
  Right non -> non
  Left err -> error $ errorBundlePretty err

type Parser = Parsec Void Text

parser :: Parser Noun
parser = try atomParser <|> cellParser

atomParser :: Parser Noun
atomParser = atom <$> decimal

cellParser :: Parser Noun
cellParser = do
  char '['
  space
  h <- parser
  space1
  t <- sepBy parser space1
  space
  char ']'
  pure $ toCell (h : t)

toCell :: [Noun] -> Noun
toCell = \case
  ~(a : rest) -> case rest of
    [b] -> cell a b
    _ -> cell a $ toCell rest
