module Parser.Literal (
  litParser
) where

import Control.Applicative ((<|>), many)
import Data.Literal (Literal(..))
import Data.Text (Text, pack)
import Parser.Core (Parser, lexeme)
import Text.Megaparsec (between, anySingleBut)
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as Lex

litParser :: Monad m => Parser m Literal
litParser = lexeme (
  fmap litint Lex.decimal
  <|> fmap (LitStr . pack) str)
  where
  litint i = LitInt i (pack $ show i)

str :: Monad m => Parser m String
str = between quote quote . many $ anySingleBut '"'
  where
  quote = char '"'
