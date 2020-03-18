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
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
