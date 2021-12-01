{-# LANGUAGE GADTs, OverloadedStrings, TypeOperators #-}
module Translator.Typechecker (
  TypeError(..),
  typecheck
) where

import Data.Text (Text)
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))

import Data.AST (AST(..), ReprAST)
import Data.Term (Term(..), WellTypedTerm(..), Typed(..), Bindings, lookupBinding)
import Data.Type (Type(..))


data TypeError loc lit where
  Mismatch :: loc -> Type loc lit a -> Type loc lit b -> TypeError loc lit
  Unbound :: loc -> Text -> TypeError loc lit
  NotApplicable :: loc -> WellTypedTerm loc lit t -> TypeError loc lit

typecheck :: (Typed lit, ReprAST lit, Term t) =>
             Bindings loc lit t ->
             AST loc lit ->
             Either (TypeError loc lit) (WellTypedTerm loc lit t)
typecheck bindings (Literal loc lit) = Right $ typed lit
typecheck bindings (Symbol loc symb) = case lookupBinding symb bindings of
                                         Nothing -> Left $ Unbound loc symb
                                         Just term -> Right term
typecheck bindings (List _ []) = Right $ WellTypedTerm TUnit $ val "()" ()
typecheck bindings (List loc (ast : asts)) = do
  WellTypedTerm headType headTerm <- typecheck bindings ast
  WellTypedTerm tailType tailTerm <- typecheck bindings (List loc asts)
  return $ WellTypedTerm (TPair headType tailType) $ cons headTerm tailTerm
typecheck bindings (App loc f args) = do
  ft <- typecheck bindings f
  typecheckApp bindings loc ft args

typecheckApp :: (Typed lit, ReprAST lit, Term t) =>
                Bindings loc lit t ->
                loc ->
                WellTypedTerm loc lit t ->
                [AST loc lit] ->
                Either (TypeError loc lit) (WellTypedTerm loc lit t)
typecheckApp bindings loc f [] = Right f
typecheckApp bindings loc ft@(WellTypedTerm fType fTerm) (arg : args) =
  case fType of
    TFun tArg tRet -> do
      WellTypedTerm argType argTerm <- typecheck bindings arg
      Refl <- unify loc tArg argType
      let fa = WellTypedTerm tRet (app fTerm argTerm)
      typecheckApp bindings loc fa args

    _ -> Left $ NotApplicable loc ft

unify :: loc -> Type loc lit a -> Type loc lit b -> Either (TypeError loc lit) (a :~: b)
unify loc ta tb = case testEquality ta tb of
  Nothing -> Left $ Mismatch loc ta tb
  Just eq -> Right eq
