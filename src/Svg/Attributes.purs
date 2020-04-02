module Lunarbox.Svg.Attributes where

import Halogen.HTML (IProp)
import Svg.Attributes as SA
import Unsafe.Coerce (unsafeCoerce)

-- There's a bug with halogen-svg which doen's allow me to use this 
-- so I made a wrapper which allows me anywhere where I can use a stroke 
strokeWidth :: forall r i. Number -> IProp ( stroke :: String | r ) i
strokeWidth = unsafeCoerce SA.strokeWidth
