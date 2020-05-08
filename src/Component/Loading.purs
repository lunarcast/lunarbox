module Lunarbox.Component.Loading
  ( loading
  ) where

import Prelude
import Data.Array (replicate)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Lunarbox.Component.Utils (className)

-- Loading animation 
-- Based on loading.io
loading :: forall h a. HTML h a
loading =
  HH.div [ className "loading-container" ]
    [ HH.div [ className "lds-roller" ]
        $ replicate 8 (HH.div_ [])
    ]
