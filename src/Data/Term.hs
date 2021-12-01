{-# LANGUAGE GADTs, OverloadedStrings #-}
module Data.Term (
  Term(..),
  Typed(..),
  WellTypedTerm(..),
  Bindings,
  builtins,
  lookupBinding
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Data.Type (Type(..))


class Term t where
  val :: Text -> a -> t a
  cons :: t a -> t b -> t (a, b)
  app :: t (a -> b) -> t a -> t b


data WellTypedTerm loc lit t where
  WellTypedTerm :: Term t => Type loc lit a -> t a -> WellTypedTerm loc lit t

class Typed t where
  typed :: Term term =>  t -> WellTypedTerm loc lit term

newtype Bindings loc lit t = Bindings (Map Text (WellTypedTerm loc lit t))

literal :: Term t => Typed lit => Text -> lit -> WellTypedTerm loc lit t
literal lit val = typed val

builtins :: Term t => Bindings loc lit t
builtins = Bindings $ Map.fromList [
    ("()", WellTypedTerm TUnit $ val "()" ()),
    ("nil", WellTypedTerm TUnit $ val "nil" ()),
    ("+", WellTypedTerm (TFun TInt (TFun TInt TInt)) $ val "+" (+))
  ]

lookupBinding :: Term t => Text -> Bindings loc lit t -> Maybe (WellTypedTerm loc lit t)
lookupBinding name (Bindings bs) = Map.lookup name bs
