let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200507/packages.dhall sha256:9c1e8951e721b79de1de551f31ecb5a339e82bbd43300eb5ccfb1bf8cf7bbd62

let overrides = {=}

let additions =
      { colehaus-graphs =
        { dependencies = [ "ordered-collections", "catenable-lists" ]
        , repo = "https://github.com/colehaus/purescript-graphs"
        , version = "v7.0.0"
        }
      , data-default =
        { dependencies = [ "maybe", "record", "psci-support", "lists" ]
        , repo = "https://github.com/thought2/purescript-data-default"
        , version = "v0.3.2"
        }
      }

in  upstream // overrides // additions
