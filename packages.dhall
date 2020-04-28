let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200423/packages.dhall sha256:c180a06bb5444fd950f8cbdd6605c644fd246deb397e62572b8f4a6b9dbcaf22

let overrides = {=}

let additions =
      { colehaus-graphs =
          { dependencies = [ "ordered-collections", "catenable-lists" ]
          , repo = "https://github.com/colehaus/purescript-graphs"
          , version = "v7.0.0"
          }
      , halogen-svg =
          { dependencies = [ "strings", "halogen", "dom-indexed" ]
          , repo = "https://github.com/statebox/purescript-halogen-svg"
          , version = "master"
          }
      , data-default =
          { dependencies = [ "maybe", "record", "psci-support", "lists" ]
          , repo = "https://github.com/thought2/purescript-data-default"
          , version = "v0.3.2"
          }
      }

in  upstream // overrides // additions
