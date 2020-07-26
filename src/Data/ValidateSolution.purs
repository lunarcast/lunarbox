module Lunarbox.Data.ValidateSolution
  ( ExpressionPack
  , Solution
  , SolutionError(..)
  , validateSolution
  ) where

import Prelude
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Traversable (for)
import Effect (Effect)
import Lunarbox.Control.Monad.Dataflow.Solve.Unify (canUnify)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))
import Lunarbox.Data.Dataflow.Type (Type(..), inputs, typeBool, typeNumber, typeString)
import Test.QuickCheck (Result(..), arbitrary, checkResults, coarbitrary, quickCheckPure', randomSeed)
import Test.QuickCheck.Gen (Gen, arrayOf, repeatable)

type ExpressionPack
  = { type' :: Type
    , value :: RuntimeValue
    }

type Solution
  = { current :: ExpressionPack
    , intended :: ExpressionPack
    }

-- | Errors which can appear when comparing an users solution
data SolutionError
  = DifferentTypes Type Type
  | CheckFailures (List String)

instance showSolutionError :: Show SolutionError where
  show (DifferentTypes a b) = "Cannot match type " <> show b <> " with type " <> show a
  -- | TODO: handle this in a more elegant way
  show (CheckFailures _) = "[quickcheck failures]"

-- | Same as arbitrary but only generates stuff of the correct type
typeToArbitrary :: Type -> Gen RuntimeValue
typeToArbitrary ty@(TConstant _ _)
  | ty == typeNumber = Number <$> arbitrary
  | ty == typeString = String <$> arbitrary
  | ty == typeBool = Bool <$> arbitrary

typeToArbitrary ty@(TConstant "Array" [ inner ]) = NArray <$> arrayOf (typeToArbitrary inner)

typeToArbitrary ty@(TConstant "Function" [ _, to ]) = Function <$> go
  where
  go = repeatable \a -> coarbitrary a $ typeToArbitrary to

typeToArbitrary _ = arbitrary

-- | Validate that a provided solution has the same behavior as the intended one.
validateSolution :: Solution -> Effect (Either SolutionError Unit)
validateSolution { current, intended }
  -- | TODO: do more than just unification
  | not $ canUnify current.type' intended.type' =
    pure
      $ Left
      $ DifferentTypes current.type' intended.type'
  | otherwise = do
    result <- go <$> randomSeed
    pure
      if List.null result.failures then
        Right unit
      else
        Left $ CheckFailures $ _.message <$> result.failures
    where
    go seed = checkResults $ quickCheckPure' seed 100 prop

    ins = inputs intended.type'

    prop = do
      vals <- for ins typeToArbitrary
      let
        currentVal = callWithVals vals current.value

        intendedVal = callWithVals vals intended.value
      pure case currentVal, intendedVal of
        Right a, Right b
          | a == b -> Success
          | otherwise -> Failed err
            where
            err = show a <> " =/= " <> show b
        Left a, _ -> Failed $ "Cannot vall non-function value " <> show a
        _, Left a -> Failed $ "Internal error: " <> show a <> " is not a function"

-- | Try calling a runtime value with an array of inputs
callWithVals :: List RuntimeValue -> RuntimeValue -> Either RuntimeValue RuntimeValue
callWithVals Nil a = Right a

callWithVals (head : tail) (Function func) = callWithVals tail (func head)

callWithVals _ func = Left func
