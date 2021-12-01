{-# LANGUAGE FlexibleInstances, GADTs, OverloadedStrings #-}
module Main where

import Control.Arrow (left)
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text
import Text.Megaparsec (parse)
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)
import Text.Megaparsec.Error (ParseErrorBundle)

import Data.Term (WellTypedTerm(..), builtins)
import Data.Type (Type)
import Data.Literal (Literal)
import Data.Repr (Repr, repr)
import Parser.Core (ParseError)
import Parser.AST (expr)
import Parser.Literal (litParser)
import Translator.Typechecker (TypeError(..), typecheck)


class Textual e where
  toText :: e -> Text

data Error where
  Error :: Textual e => e -> Error

instance Textual (Type loc lit a) where
  toText = pack . show

instance Textual SourcePos where
  toText = pack . show

instance Textual loc => Textual (TypeError loc a) where
  toText (Mismatch loc ta tb) = "Type Error at " <> toText loc
                                 <> ":\n  Expected: " <> toText ta
                                 <> "\n  Found: " <> toText tb <> "."
  toText (Unbound loc symbol) = "Unbound variable: " <> symbol
                                <> " at: " <> toText loc <> "."
  toText (NotApplicable loc (WellTypedTerm ty term)) =
    "Not a function " -- <> repr term
    <>" at: " <> toText loc
    <> ". Cannot be applied."

instance Show e => Textual (ParseErrorBundle Text e) where
  toText = pack . show


main :: IO ()
main = Text.interact compileAndRepr >> putStrLn ""


compileAndRepr :: Text -> Text
compileAndRepr input = case interpret input of
  Right (WellTypedTerm ty t) -> repr t
  Left (Error e) -> toText e

interpret :: Text -> Either Error (WellTypedTerm SourcePos Literal Repr)
interpret input = do
  ast <- left Error $ parse (expr litParser) "/dev/stdin" input
  left Error $ typecheck builtins ast
