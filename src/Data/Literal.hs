{-# LANGUAGE GADTs #-}
module Data.Literal (
  Literal(..)
) where

import Data.Text (Text)

import Data.AST (ReprAST(..))
import Data.Term (Term(..), Typed(..), WellTypedTerm(..))
import Data.Type (Type(..))

data Literal = LitInt Integer Text
             | LitStr Text
  deriving Show


instance Typed Literal where
  typed (LitInt i orig) = WellTypedTerm TInt $ val orig i
  typed (LitStr s) = WellTypedTerm TString $ val s s

instance ReprAST Literal where
  reprAST (LitInt _ orig) = orig
  reprAST (LitStr s) = s
