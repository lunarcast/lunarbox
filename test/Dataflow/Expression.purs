module Test.Dataflow.Expression (spec) where

import Prelude
import Lunarbox.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Dataflow.Type (typeBool, typeInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "The NativeExpression eq instance" do
    it "should not return true if the functions match but the types doesn't" do
      let
        ne = NativeExpression typeInt identity

        ne' = NativeExpression typeBool identity

        result = ne == ne'
      result `shouldEqual` false
    it "should be true if the functions don't match but the types do" do
      let
        ne = NativeExpression typeInt identity

        ne' :: NativeExpression Boolean Boolean
        ne' = NativeExpression typeInt not

        result = ne == ne'
      result `shouldEqual` true
