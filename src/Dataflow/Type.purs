module Lunarbox.Dataflow.Type (Type(..), TVar(..), typeInt, typeBool, Scheme(..)) where

import Prelude
import Data.Foldable (fold)

newtype TVar
  = TV String

derive instance tvarEq :: Eq TVar

derive instance tvarOrd :: Ord TVar

instance tvarShow :: Show TVar where
  show (TV name) = name

data Type
  = TConstant String
  | TArrow Type Type
  | TVarariable TVar

derive instance typeEq :: Eq Type

derive instance typeOrd :: Ord Type

isArrow :: Type -> Boolean
isArrow = case _ of
  TArrow _ _ -> true
  _ -> false

printType :: Boolean -> Type -> String
printType _ (TVarariable v) = show v

printType _ (TConstant s) = s

printType p (TArrow from to) = if p then "(" <> result <> ")" else result
  where
  prefix = printType (isArrow from) from

  result = prefix <> " -> " <> show to

instance typeShow :: Show Type where
  show = printType false

typeInt :: Type
typeInt = TConstant "Int"

typeBool :: Type
typeBool = TConstant "Bool"

data Scheme
  = Forall (Array TVar) Type

instance showScheme :: Show Scheme where
  show (Forall [] t) = show t
  show (Forall quantifiers t) = "forall" <> fold (quantifiers <#> (\(TV n) -> " " <> n)) <> ". " <> show t
