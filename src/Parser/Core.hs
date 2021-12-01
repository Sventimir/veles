{-# LANGUAGE OverloadedStrings #-}
module Parser.Core (
  Parser,
  ParseError(..),
  lexeme
) where

import Data.Text (Text, unpack)
import Text.Megaparsec (ParsecT)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Error (ShowErrorComponent(..))


data ParseError = UnexpectedToken Text
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ParseError where
  errorComponentLen e = length $ showErrorComponent e
  showErrorComponent (UnexpectedToken t) = "Unexpected token: '" <> unpack t <> "'."

type Parser m a = ParsecT ParseError Text m a


space :: Monad m => Parser m ()
space = Lex.space
  space1
  (Lex.skipLineComment "#")
  (Lex.skipBlockComment "{#" "#}")

lexeme :: Monad m => Parser m a -> Parser m a
lexeme = Lex.lexeme space
