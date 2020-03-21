module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Dataflow.Expressible (spec) as Expressible
import Test.Dataflow.Expression (spec) as Expression

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        Expressible.spec
        Expression.spec
