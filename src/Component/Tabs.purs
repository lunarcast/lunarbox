module Lunarbox.Component.Tabs where

import Prelude
import Control.MonadZero (guard)
import Data.Foldable (find)
import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (className, maybeElement)

type Input h t
  = { tabs ::
      Array
        { name :: t
        , content :: h
        }
    , currentTab :: t
    , headerStart :: Maybe h
    , headerEnd :: Maybe h
    }

-- | Standard reusable tabs component. The state needs to be handled outside this.
component :: forall h a t. Show t => Eq t => Input (HH.HTML h a) t -> HH.HTML h a
component { tabs, currentTab, headerStart, headerEnd } =
  HH.div [ className "tabs" ]
    [ HH.header [ className "tabs__header" ] header
    , HH.main [ className "tabs__content" ] [ maybeElement maybeContent \{ content } -> content ]
    ]
  where
  tabHtml { name } = HH.div [ HP.classes $ HH.ClassName <$> classes ] [ HH.text (show name) ]
    where
    classes = [ "tabs__tab" ] <> currentTabClass

    currentTabClass = "tabs__tab--current" <$ guard (name == currentTab)

  header = start <> (tabHtml <$> tabs) <> end
    where
    mkPiece = maybe [] pure

    start = mkPiece headerStart

    end = mkPiece headerEnd

  maybeContent = find (\{ name } -> name == currentTab) tabs
