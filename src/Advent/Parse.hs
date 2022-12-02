module Advent.Parse
  ( parseOrDie
  , parseFile
  , parseAll
  , token
  , sym
  , twoNewlines
  , oneSpace
  , module X
  ) where

import Advent.Prelude

import Data.Attoparsec.Text as X

parseOrDie :: Parser a -> Text -> IO a
parseOrDie parser = either die pure . parseAll parser

parseFile :: Parser a -> FilePath -> IO a
parseFile parser = parseOrDie parser <=< readFile

parseAll :: Parser a -> Text -> Either Text a
parseAll parser = mapLeft pack . parseOnly (trim parser <* endOfInput)

trim :: Parser a -> Parser a
trim parser = skipSpace *> parser <* skipSpace

token :: Parser a -> Parser a
token parser = parser <* skipSpace

sym :: Text -> Parser ()
sym = void . token . string

oneSpace :: Parser ()
oneSpace = void (char ' ') <|> endOfLine

twoNewlines :: Parser ()
twoNewlines = endOfLine *> endOfLine
