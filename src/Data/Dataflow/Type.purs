module Lunarbox.Data.Dataflow.Type
  ( TVarName(..)
  , Type(..)
  , typeNumber
  , inputs
  , output
  , typeBool
  , typeString
  , numberOfInputs
  , typeFunction
  ) where

import Prelude
import Data.List (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.String (joinWith)
import Lunarbox.Data.Char (arrow)
import Lunarbox.Data.String (spaced)

newtype TVarName
  = TVarName String

derive instance eqTVarName :: Eq TVarName

derive instance ordTVarName :: Ord TVarName

derive instance newtypeTVarName :: Newtype TVarName _

derive newtype instance semigroupTVarName :: Semigroup TVarName

instance tvarShow :: Show TVarName where
  show = unwrap

data Type
  = TConstant String (Array Type)
  | TVariable Boolean TVarName

-- Primitive types
typeNumber :: Type
typeNumber = TConstant "Number" []

typeBool :: Type
typeBool = TConstant "Bool" []

typeString :: Type
typeString = TConstant "String" []

typeFunction :: Type -> Type -> Type
typeFunction from to = TConstant "Function" [ from, to ]

-- Internal version of numberOfInputs which also takes an argument for the accumulated count
numberOfInputs' :: Int -> Type -> Int
numberOfInputs' count = case _ of
  TConstant "Function" [ _, t ] -> numberOfInputs' (count + 1) t
  _ -> count

-- Returns the number of inputs a function with this type would have
numberOfInputs :: Type -> Int
numberOfInputs = numberOfInputs' 0

-- Get all the inputs of a type
-- Eg: a -> b -> c will return [a, b]
inputs :: Type -> List Type
inputs (TConstant "Function" [ i, t ]) = i : inputs t

inputs _ = Nil

-- Get the output of a type
output :: Type -> Type
output (TConstant "Function" [ _, outputType ]) = output outputType

output type' = type'

derive instance typeEq :: Eq Type

instance typeShow :: Show Type where
  show = printType false

printType :: Boolean -> Type -> String
printType _ (TVariable _ v) = show v

printType p (TConstant "Function" [ from, to ]) = if p then "(" <> result <> ")" else result
  where
  isArrow = case from of
    TConstant "Function" _ -> true
    _ -> false

  prefix = printType isArrow from

  result = prefix <> spaced arrow <> show to

printType _ (TConstant name vars) = name <> " " <> (joinWith " " $ show <$> vars)
