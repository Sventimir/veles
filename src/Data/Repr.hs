{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Data.Repr (
  Repr,
  repr,
  showTypeOf
) where

import Data.Text (Text, unpack)

import Data.Term (Term(..), WellTypedTerm(..))


newtype Repr a = Repr Text

repr :: Repr a -> Text
repr (Repr t) = t

instance Term Repr where
  val name _ = Repr name
  cons (Repr left) (Repr right) = Repr ("(" <> left <> ", " <> right <> ")")
  app (Repr f) (Repr a) = Repr ("(" <> f <> " " <> a <> ")")

instance Show (WellTypedTerm loc lit Repr) where
  show (WellTypedTerm _ (Repr r)) = unpack r

showTypeOf :: WellTypedTerm loc lit t -> String
showTypeOf (WellTypedTerm t _) = show t
