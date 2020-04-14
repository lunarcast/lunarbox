module Lunarbox.Data.Dataflow.Type
  ( TVarName(..)
  , Type(..)
  , typeNumber
  , typeBool
  , typeString
  , numberOfInputs
  ) where

import Prelude
import Data.Newtype (class Newtype, unwrap)

newtype TVarName
  = TVarName String

derive instance eqTVarName :: Eq TVarName

derive instance ordTVarName :: Ord TVarName

derive instance newtypeTVarName :: Newtype TVarName _

instance tvarShow :: Show TVarName where
  show = unwrap

data Type
  = TConstant String
  | TArrow Type Type
  | TVarariable TVarName

-- Primitive types
typeNumber :: Type
typeNumber = TConstant "Number"

typeBool :: Type
typeBool = TConstant "Bool"

typeString :: Type
typeString = TConstant "String"

-- Internal version of numberOfInputs which also takes an argument for the accumulated count
numberOfInputs' :: Int -> Type -> Int
numberOfInputs' count = case _ of
  TArrow _ t -> numberOfInputs' (count + 1) t
  _ -> count

-- Returns the number of inputs a function with this type would have
numberOfInputs :: Type -> Int
numberOfInputs = numberOfInputs' 0

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
