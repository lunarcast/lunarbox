module Lunarbox.Component.Switch
  ( Input
  , switchHeight
  , switchWidth
  , switch
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Maybe (Maybe)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onChecked)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (className)

-- Sizes of the switch. Usefull for using inside a foreidObject
switchHeight :: Number
switchHeight = 28.0

switchWidth :: Number
switchWidth = 54.0

type Input
  = { checked :: Boolean
    , round :: Boolean
    }

-- Simple switch element used for the boolean node
switch :: forall h a. Input -> (Boolean -> Maybe a) -> HTML h a
switch { checked, round } handleCheck =
  HH.label [ className "switch" ]
    [ HH.input
        [ HP.type_ HP.InputCheckbox
        , HP.checked checked
        , onChecked handleCheck
        ]
    , HH.span [ HP.classes $ ClassName <$> [ "switch-slider" ] <> ("round" <$ guard round) ] []
    ]
