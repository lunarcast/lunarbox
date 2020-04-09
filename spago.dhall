{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "lunarbox"
, dependencies =
  [ "aff"
  , "aff-bus"
  , "colehaus-graphs"
  , "console"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "halogen-svg"
  , "halogen-vdom"
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
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
