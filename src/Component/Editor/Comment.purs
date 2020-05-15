module Lunarbox.Component.Editor.Comment
  ( comment
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown, onMouseMove, onWheel)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (className)
import Lunarbox.Data.Editor.Constants (commentTextMargin)
import Lunarbox.Data.Vector (Vec2)
import Svg.Attributes as SA
import Svg.Elements as SE
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.WheelEvent as WheelEvent

-- Half the margin aroudn the textbox
halfMargin :: Number
halfMargin = commentTextMargin / 2.0

type Actions a
  = { select :: Event -> Maybe a
    , stopPropagation :: Event -> Maybe a
    }

type Input
  = { position :: Vec2 Number
    , scale :: Vec2 Number
    , text :: String
    }

-- Comment nodes
comment :: forall h a. Input -> Actions a -> HTML h a
comment { position, scale, text } { select, stopPropagation } =
  SE.g
    [ SA.transform
        [ SA.Translate (position !! d0) (position !! d1)
        ]
    , SA.class_ "comment-container"
    , onMouseDown $ select <<< MouseEvent.toEvent
    ]
    [ SE.rect
        [ SA.width $ scale !! d0
        , SA.height $ scale !! d1
        , SA.class_ "comment"
        , SA.ry 10.0
        ]
    , SE.foreignObject
        [ SA.width $ scale !! d0 - commentTextMargin
        , SA.height $ scale !! d1 - commentTextMargin
        , SA.x halfMargin
        , SA.y halfMargin
        ]
        [ HH.textarea
            [ HP.value text
            , className "comment-textarea"
            , onMouseDown $ stopPropagation <<< MouseEvent.toEvent
            , onMouseMove $ stopPropagation <<< MouseEvent.toEvent
            , onWheel $ stopPropagation <<< WheelEvent.toEvent
            ]
        ]
    ]
