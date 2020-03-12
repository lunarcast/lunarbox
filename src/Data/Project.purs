module Lunarbox.Data.Project where

import Prelude
import Control.Monad.List.Trans (filter)
import Data.Graph as G
import Data.List (List, foldl, reverse, (\\))
import Data.Map (Map)
import Data.Maybe (Maybe)
import Dataflow.Expression (Expression(..))
import Dataflow.Type (TVar(..))
import Lunarbox.Dataflow.Expression (NativeExpression)

newtype NodeId
  = NodeId Int

derive instance eqNodeId :: Eq NodeId

derive instance ordNodeId :: Ord NodeId

newtype FunctionName
  = FunctionName String

derive instance eqFunctionName :: Eq FunctionName

derive instance ordFunctionName :: Ord FunctionName

data Node
  = InputNode
  | ComplexNode
    { connectedTo :: Array (Maybe NodeId)
    , function :: FunctionName
    }
  | OutputNode (Maybe NodeId)

newtype NodeGroup
  = NodeGroup
  { visible :: Boolean
  , inputs :: List NodeId
  , nodes :: G.Graph NodeId Node
  , output :: NodeId
  }

data VisualFunction
  = NativeVF (forall a b. NativeExpression a b)
  | DataflowFunction NodeGroup

type Project
  = { functions :: Map FunctionName VisualFunction
    , main :: FunctionName
    }

orderNodes :: NodeGroup -> List NodeId
orderNodes (NodeGroup function) = G.topologicalSort function.nodes

functionDeclaration :: List TVar -> Expression -> Expression
functionDeclaration params expr = foldl (flip Lambda) expr $ reverse $ params

compileFunction :: Project -> NodeGroup -> Expression
compileFunction project group@(NodeGroup function) = do
  let
    ordered = orderNodes group

    returnV = ordered \\ function.inputs
  Variable $ TV $ "shut up"
 {--compileProject :: Project -> Maybe Expression
 compileProject project = do
   main <- Map.lookup project.main project.functions
   pure $ Literal $ LInt $ 7
-