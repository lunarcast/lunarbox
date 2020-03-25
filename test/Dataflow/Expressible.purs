module Test.Dataflow.Expressible (spec) where

import Prelude
import Data.Maybe (Maybe(..))
import Lunarbox.Dataflow.Expressible (nullExpr, toExpression)
import Lunarbox.Dataflow.Expression (Expression(..), Literal(..))
import Lunarbox.Dataflow.Type (TVar(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "The Expressible typeclass" do
    describe "The String instance" do
      it "should create a variable with the string as the name" do
        let
          name = "myVariableName"

          expression = toExpression name
        expression `shouldEqual` (Variable $ TV $ name)
    describe "The Maybe instance" do
      it "should return the null expression when given Nothing" do
        let
          expression = toExpression (Nothing :: Maybe Expression)
        expression `shouldEqual` nullExpr
      it "should return the inner value when given Just" do
        let
          inner = Literal $ LInt 7

          expression = toExpression $ Just inner
        expression `shouldEqual` inner
    describe "The Expression instance" do
      it "should just return the given expression" do
        let
          expression = Literal $ LInt 7
        expression `shouldEqual` (toExpression expression)
