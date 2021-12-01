{-# LANGUAGE GADTs #-}
module Data.Type (
  Type(..)
) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))
import Data.AST (AST)


-- The type parameter indicates into what Haskell type given type maps.
data Type loc lit a where
  TUnit :: Type loc lit ()
  TBool :: Type loc lit Bool
  TNat :: Type loc lit Integer
  TInt :: Type loc lit Integer
  TFloat :: Type loc lit Double
  TSymbol :: Type loc lit Text
  TChar :: Type loc lit Char
  TString :: Type loc lit Text
  TLoc :: Type loc lit loc
  TLit :: Type loc lit lit
  TAstLit :: Type loc lit (AST loc lit)
  TPair :: Type loc lit a -> Type loc lit b -> Type loc lit (a, b)
  TUnion :: Type loc lit a -> Type loc lit b -> Type loc lit (Either a b)
  TArray :: Type loc lit a -> Type loc lit [a]
  TFun :: Type loc lit a -> Type loc lit b -> Type loc lit (a -> b)

instance Show (Type loc lit a) where
  show TBool = "Bool"
  show TNat = "Nat"
  show TInt = "Int"
  show TFloat = "Float"
  show TSymbol = "Symbol"
  show TChar = "Char"
  show TString = "String"
  show TLoc = "Location"
  show TLit = "Literal"
  show TAstLit = "AST"
  show (TPair l r) = "(" <> show l <> ", " <> show r <> ")"
  show (TUnion l r) = "(" <> show l <> " | " <> show r <> ")"
  show (TArray t) = "[" <> show t <> "]"
  show (TFun a r) = show a <> " -> " <> show r

instance TestEquality (Type loc lit) where
  testEquality TUnit TUnit = Just Refl
  testEquality TBool TBool = Just Refl
  testEquality TNat TNat = Just Refl
  testEquality TInt TInt = Just Refl
  testEquality TFloat TFloat = Just Refl
  testEquality TSymbol TSymbol = Just Refl
  testEquality TChar TChar = Just Refl
  testEquality TString TString = Just Refl
  testEquality TLoc TLoc = Just Refl
  testEquality TLit TLit = Just Refl
  testEquality TAstLit TAstLit = Just Refl
  testEquality (TPair lfst lsnd) (TPair rfst rsnd) = do
    Refl <- testEquality lfst rfst
    Refl <- testEquality lsnd rsnd
    return Refl
  testEquality (TUnion lleft lright) (TUnion rleft rright) = do
    Refl <- testEquality lleft rleft
    Refl <- testEquality lright rright
    return Refl
  testEquality (TArray larg) (TArray rarg) = do
    Refl <- testEquality larg rarg
    return Refl
  testEquality (TFun larg lres) (TFun rarg rres) = do
    Refl <- testEquality larg rarg
    Refl <- testEquality lres rres
    return Refl
  testEquality _ _ = Nothing
