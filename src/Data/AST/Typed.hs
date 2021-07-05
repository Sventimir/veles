{-# LANGUAGE GADTs #-}
module Data.AST.Typed (
  Expr(..)
) where

import Data.Text (Text)


newtype Symbol = Symbol Text

data Expr a where
  EInt :: Integer -> Expr Integer
  EFloat :: Double -> Expr Double
  EChar :: Char -> Expr Char
  EStr :: Text -> Expr Text
  ESymbol :: Symbol -> Expr Symbol
  EList :: [a] -> Expr [a]
  EFun :: (a -> b) -> Expr (a -> b)
  EApp :: Expr (a -> b) -> Expr a -> Expr b


eval :: Expr a -> a
eval (EInt i) = i
eval (EFloat f) = f
eval (EChar c) = c
eval (EStr s) = s
eval (ESymbol s) = s
eval (EList l) = l
eval (EFun f) = f
eval (EApp f a) = (eval f) (eval a)
