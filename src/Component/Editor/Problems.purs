module Lunarbox.Component.Editor.Problems where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (className, whenElem)
import Lunarbox.Data.Dataflow.Expression.Lint (LintError)
import Lunarbox.Data.Dataflow.Expression.Lint as LintError
import Lunarbox.Data.Dataflow.TypeError (TypeError)
import Lunarbox.Data.Dataflow.TypeError as TypeError
import Lunarbox.Data.Editor.Location (Location(..))

type ProblemsInput a
  = { typeErrors :: Array (TypeError Location)
    , lintingErrors :: Array (LintError Location)
    , navigateTo :: Location -> a
    }

-- Icons
errorIcon :: forall h a. HH.HTML h a
errorIcon = icon "error"

warningIcon :: forall h a. HH.HTML h a
warningIcon = icon "warning"

-- The actual component
problems :: forall h a. ProblemsInput a -> HH.HTML h a
problems { typeErrors: [], lintingErrors: [] } = HH.main [ className "problems__empty" ] [ HH.text "No errors here!" ]

problems { typeErrors, lintingErrors, navigateTo } =
  HH.main [ className "problems__container" ]
    $ typeErrorHtml
    <> lintingErrorHtml
  where
  mkProblem = createProblemMaker navigateTo

  typeErrorHtml =
    ( \error ->
        mkProblem
          { level: Error
          , message: TypeError.printError error
          , location: TypeError.getLocation error
          }
    )
      <$> typeErrors

  lintingErrorHtml =
    ( \error ->
        mkProblem
          { level: Warning
          , message: "not here yet"
          , location: LintError.getLocation error
          }
    )
      <$> lintingErrors

-- | Data about how to color a problem
data ProblemLevel
  = Warning
  | Error

type ProblemConfig
  = { level :: ProblemLevel, message :: String, location :: Location
    }

-- | Create the html for a problem
createProblemMaker :: forall h a. (Location -> a) -> ProblemConfig -> HH.HTML h a
createProblemMaker navigateTo { location, message, level } =
  whenElem (location /= UnknownLocation) \_ ->
    HH.section
      [ className $ "problems__card "
          <> case level of
              Warning -> "problems__card--warning"
              Error -> "problems__card--error"
      ]
      [ HH.div [ className "problems__card-header" ]
          [ HH.div [ className "problems__card-icon" ]
              [ case level of
                  Warning -> warningIcon
                  Error -> errorIcon
              ]
          , HH.button
              [ className "problems__card-location"
              , onClick $ const $ Just $ navigateTo location
              ]
              [ HH.text $ show location
              ]
          ]
      , HH.div [ className "problems__card-message" ]
          [ HH.text message ]
      ]
