{-# LANGUAGE GADTs, OverloadedStrings #-}
module Parser.AST (
 expr
 -- parse
) where

import Control.Applicative ((<|>), many)
import Data.Text (Text, pack)
import Text.Megaparsec (ParsecT, SourcePos, between, oneOf, getSourcePos)
import Text.Megaparsec.Char (space1, char, letterChar, alphaNumChar, string)

import Data.AST (AST(..))
import Data.Type (Type(..))

import Parser.Core (Parser, lexeme)


type AstParser m lit = Parser m (AST SourcePos lit)


app :: Monad m => AstParser m lit -> AstParser m lit
app exprParser = lexeme $ do
  loc <- getSourcePos
  es <- between (char '(') (char ')') $ many exprParser
  case es of
    [] -> return $ List loc []
    e : es -> return $ App loc e es

quotedList :: Monad m => AstParser m lit -> AstParser m lit
quotedList exprParser = lexeme $ do
   loc <- getSourcePos
   exprs <- between (string "'(") (char ')') $ many exprParser
   return $ List loc exprs

symbol :: Monad m => AstParser m lit
symbol = lexeme $ do
  loc <- getSourcePos
  hd <- (letterChar <|> primSymbol)
  tl <- many (alphaNumChar <|> primSymbol)
  return . Symbol loc $ pack (hd : tl)
    where
    primSymbol = oneOf ['~', '`', '\'', '@', '$', '%', '^', '&', '*', '-',
                        '+', '_', '=', '|', ':', ',', '/', '?', '<', '>']

wrapLit :: Monad m => Parser m lit -> AstParser m lit
wrapLit lit = do
  loc <- getSourcePos
  l <- lit
  return $ Literal loc l

expr :: Monad m => Parser m lit -> Parser m (AST SourcePos lit)
expr lit = self
  where
  self = lexeme (app self <|> quotedList self <|> wrapLit lit <|> symbol)
