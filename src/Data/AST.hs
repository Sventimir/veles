{-# LANGUAGE GADTs #-}
module Data.AST (
  AST(..),
  ReprAST(..)
) where

import Data.List (intercalate)
import Data.Text (Text, unpack)


data AST loc lit where
  Literal :: loc -> lit -> AST loc lit
  Symbol :: loc -> Text -> AST loc lit
  List :: loc -> [AST loc lit] -> AST loc lit
  App :: loc -> AST loc lit -> [AST loc lit] -> AST loc lit

instance (Show lit) => Show (AST loc lit) where
  show (Literal _ lit) = show lit
  show (Symbol _ s) = unpack s
  show (List _ l) = "'(" <> (intercalate " " $ map show l) <> ")"
  show (App _ f args) = "(" <> (intercalate " " $ map show (f : args)) <> ")"

class ReprAST r where
  reprAST :: r -> Text
