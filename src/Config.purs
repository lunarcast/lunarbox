module Lunarbox.Config (Config, EnvType(..)) where

data EnvType
  = Production
  | Development

type Config
  = { envType :: EnvType
    }
