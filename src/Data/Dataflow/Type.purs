module Lunarbox.Data.Dataflow.Type
  ( TVarName(..)
  , Type(..)
  , typeNumber
  , typeBool
  ) where

import Prelude

newtype TVarName
  = TVarName String

derive instance eqTVarName :: Eq TVarName

derive instance ordTVarName :: Ord TVarName

derive newtype instance tvarShow :: Show TVarName

data Type
  = TConstant String
  | TArrow Type Type
  | TVarariable TVarName

typeNumber :: Type
typeNumber = TConstant "Number"

typeBool :: Type
typeBool = TConstant "Bool"

derive instance typeEq :: Eq Type

instance typeShow :: Show Type where
  show = printType false

printType :: Boolean -> Type -> String
printType _ (TVarariable v) = show v

printType _ (TConstant s) = s

printType p (TArrow from to) = if p then "(" <> result <> ")" else result
  where
  isArrow = case from of
    TArrow _ _ -> true
    _ -> false

  prefix = printType isArrow from

  result = prefix <> " -> " <> show to
