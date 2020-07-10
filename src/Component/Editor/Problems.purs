module Lunarbox.Component.Editor.Problems where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (className, whenElem)
import Lunarbox.Data.Dataflow.Expression.Lint (LintError, LEFormattingData)
import Lunarbox.Data.Dataflow.Expression.Lint as LintError
import Lunarbox.Data.Dataflow.TypeError (TypeError)
import Lunarbox.Data.Dataflow.TypeError as TypeError
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Location (Location(..))
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..), ScopedLocation(..))

type ProblemsInput a
  = { typeErrors :: Array (TypeError Location)
    , lintingErrors :: Array (LintError Location)
    , navigateTo :: Location -> a
    , isInternal :: FunctionName -> Boolean
    }

-- Icons
errorIcon :: forall h a. HH.HTML h a
errorIcon = icon "error"

warningIcon :: forall h a. HH.HTML h a
warningIcon = icon "warning"

-- The actual component
problems :: forall h a. ProblemsInput a -> HH.HTML h a
problems { typeErrors: [], lintingErrors: [] } = HH.main [ className "problems__empty" ] [ HH.text "No errors here!" ]

problems { typeErrors, lintingErrors, navigateTo, isInternal } =
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

  lintingErrorHtml = makeLintingErrorHtml <$> lintingErrors

  makeLintingErrorHtml error =
    whenElem shouldRender \_ ->
      mkProblem
        { level: Warning
        , message: LintError.printError (locationLEF location) error
        , location
        }
    where
    location = LintError.getLocation error

    shouldRender = case location of
      UnknownLocation -> false
      (AtFunction name) -> not $ isInternal name
      (InsideFunction name _) -> not $ isInternal name
      (AtFunctionDeclaration name) -> not $ isInternal name
      (FixpointOperator name) -> not $ isInternal name

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

-- | Linting error formatter based on the location of the error
locationLEF :: Location -> LEFormattingData
locationLEF UnknownLocation =
  { nth: Nothing
  , who: "unknown"
  , namedWho: \n -> "Variable " <> n
  }

locationLEF (FixpointOperator name) =
  { nth: Nothing
  , who: "Recursive function " <> show name
  , namedWho: \n -> "Recrusive function " <> n
  }

locationLEF (AtFunction name) =
  { nth: Nothing
  , who: "Function " <> show name
  , namedWho: \n -> "Function " <> n
  }

locationLEF (AtFunctionDeclaration name) = locationLEF (AtFunction name)

locationLEF (InsideFunction name PlaceholderPosition) = locationLEF (AtFunction name)

locationLEF (InsideFunction _ deep) = deepLocationLEF deep
  where
  deepLocationLEF (NodeLocation id) =
    { nth: Nothing
    , who: "Node " <> show id
    , namedWho: \n -> "Node " <> n
    }

  deepLocationLEF (PinLocation id (InputPin index)) =
    (deepLocationLEF (NodeLocation id))
      { nth = Just index
      }

  deepLocationLEF (FunctionUsage name) =
    { nth: Nothing
    , who: "A " <> show name <> " node"
    , namedWho: \n -> "Node " <> n <> " using function " <> show name
    }

  deepLocationLEF InsideNative =
    { nth: Nothing
    , who: "A native function"
    , namedWho: \n -> "Function " <> show n
    }

  deepLocationLEF (PinLocation id OutputPin) = deepLocationLEF (NodeLocation id)

  deepLocationLEF (NodeDefinition id) = deepLocationLEF (NodeLocation id)

  deepLocationLEF (AtApplication id index) = deepLocationLEF (PinLocation id (InputPin index))

  deepLocationLEF FunctionDeclaration = locationLEF UnknownLocation

  deepLocationLEF (UnexistingNode _) = locationLEF UnknownLocation

  deepLocationLEF PlaceholderPosition = locationLEF UnknownLocation
