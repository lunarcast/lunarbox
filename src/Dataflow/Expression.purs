module Lunarbox.Dataflow.Expression where

import Prelude
import Lunarbox.Dataflow.Type (TVar, Type)

data Literal
  = LInt Int
  | LBool Boolean

derive instance literalEq :: Eq Literal

derive instance literalOrd :: Ord Literal

instance showLiteral :: Show Literal where
  show (LInt i) = show i
  show (LBool b) = show b

data NativeExpression a b
  = NativeExpression Type (a -> b)

instance eqNativeExpression :: Eq (NativeExpression a b) where
  eq (NativeExpression t _) (NativeExpression t' _) = t == t'

data Expression
  = Variable TVar
  | FunctionCall Expression Expression
  | Lambda TVar Expression
  | Literal Literal
  | Let TVar Expression Expression
  | If Expression Expression Expression
  | FixPoint Expression
  | Native (forall a b. NativeExpression a b)

derive instance expressinEq :: Eq Expression

-- POC codegen for easy printing
instance showExpression :: Show Expression where
  show (Variable v) = show v
  show (FunctionCall f i) = show f <> " " <> show i
  show (Lambda arg body) = "\\" <> show arg <> " -> " <> show body
  show (Literal lit) = show lit
  show (Let name value body) = "let " <> show name <> " = " <> show value <> " in " <> show body
  show (If expr then' else') = "if " <> show expr <> " then " <> show then' <> " else " <> show else'
  show (FixPoint expr) = "<Recursive>( " <> show expr <> " )"
  show (Native (NativeExpression t _)) = "<Native>( " <> show t <> " )"
