module Lunarbox.Data.Editor.Node.CommentData
  ( CommentData(..)
  , minCommentScale
  , _CommentDataText
  , _CommentDataScale
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Vec (vec2)
import Lunarbox.Data.Lens (newtypeIso)
import Lunarbox.Data.Vector (Vec2)

-- THis is some extra node data for components
newtype CommentData
  = CommentData
  { text :: String
  , scale :: Vec2 Number
  }

-- We don't want a tiny comment when there's a small amount of text
minCommentScale :: Vec2 Number
minCommentScale = vec2 200.0 150.0

-- Typeclasses instances
derive instance eqCommentData :: Eq CommentData

derive instance newtypeCommentData :: Newtype CommentData _

derive instance genericCommentData :: Generic CommentData _

derive newtype instance encodeJsonCommentData :: EncodeJson CommentData

derive newtype instance decodeJsonCommentData :: DecodeJson CommentData

instance defaultCommentData :: Default CommentData where
  def = CommentData { text: "This is a comment!", scale: minCommentScale }

instance showCommentData :: Show CommentData where
  show = genericShow

-- Lenses
_CommentDataText :: Lens' CommentData String
_CommentDataText = newtypeIso <<< prop (SProxy :: _ "text")

_CommentDataScale :: Lens' CommentData (Vec2 Number)
_CommentDataScale = newtypeIso <<< prop (SProxy :: _ "scale")
