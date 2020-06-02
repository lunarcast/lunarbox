module Lunarbox.Foreign.Render where

import Prelude
import Data.Nullable (Nullable)
import Effect (Effect)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Vector (Vec2)
import Web.HTML (HTMLCanvasElement)

foreign import data Context2d :: Type

foreign import getContext :: HTMLCanvasElement -> Effect Context2d

type InputData
  = { output :: Nullable String
    , color :: String
    , arc :: Vec2 Number
    , value :: Nullable NodeId
    }

type NodeData
  = { position :: Vec2 Number
    , inputs :: Array (Array InputData)
    }

type SceneData
  = { nodes :: Array NodeData
    }

type App
  = { production :: Boolean
    , renderScene :: Context2d -> SceneData -> Effect Unit
    }
