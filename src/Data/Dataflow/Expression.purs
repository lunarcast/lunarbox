module Lunarbox.Data.Dataflow.Expression
  ( Literal(..)
  , NativeExpression(..)
  , Expression(..)
  , VarName(..)
  , functionDeclaration
  ) where

import Prelude
import Data.List (List, foldr)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Type (Type)

newtype VarName
  = VarName String

derive instance eqVarName :: Eq VarName

derive newtype instance showVarName :: Show VarName

data Literal
  = LInt Int
  | LBool Boolean

derive instance literalEq :: Eq Literal

data NativeExpression
  = NativeExpression Type RuntimeValue

derive instance eqNativeExpression :: Eq NativeExpression

data Expression l
  = Variable l VarName
  | FunctionCall l (Expression l) (Expression l)
  | Lambda l VarName (Expression l)
  | Literal l Literal
  | Let l VarName (Expression l) (Expression l)
  | If l (Expression l) (Expression l) (Expression l)
  | FixPoint l (Expression l)
  | Native l NativeExpression

derive instance expressinEq :: Eq l => Eq (Expression l)

derive instance functorExpression :: Functor Expression

-- Takes a list of argument names and a body and creates the body of a function
functionDeclaration :: forall l. l -> Expression l -> List VarName -> Expression l
functionDeclaration = foldr <<< Lambda
