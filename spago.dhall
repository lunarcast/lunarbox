{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "lunarbox"
, dependencies =
  [ "aff"
  , "aff-bus"
  , "arrays"
  , "colehaus-graphs"
  , "console"
  , "data-default"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "halogen-svg"
  , "halogen-vdom"
  , "lists"
  , "math"
  , "maybe"
  , "ordered-collections"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "routing"
  , "routing-duplex"
  , "sized-vectors"
  , "spec"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
