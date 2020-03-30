module Lunarbox.Control.Monad.Dataflow.Infer where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (Except)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.State (class MonadState)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Lunarbox.Data.Dataflow.Constraint (ConstraintSet)
import Lunarbox.Dataflow.Error (TypeError)
import Lunarbox.Dataflow.TypeEnv (TypeEnv)

newtype InferState
  = InferState
  { count :: Int
  }

newtype InferEnv l
  = InferEnv
  { typeEnv :: TypeEnv
  , location :: l
  }

newtype Infer l a
  = Infer (RWST (InferEnv l) ConstraintSet InferState (Except (TypeError l)) a)

derive newtype instance functorInfer :: Functor (Infer l)

derive newtype instance applyInfer :: Apply (Infer l)

derive newtype instance applicativeInfer :: Applicative (Infer l)

derive newtype instance bindInfer :: Bind (Infer l)

derive newtype instance monadInfer :: Monad (Infer l)

derive newtype instance monadAskInfer :: MonadAsk (InferEnv l) (Infer l)

derive newtype instance monadReaderInfer :: MonadReader (InferEnv l) (Infer l)

derive newtype instance monadTellInfer :: MonadTell ConstraintSet (Infer l)

derive newtype instance monadWriterInfer :: MonadWriter ConstraintSet (Infer l)

derive newtype instance monadStateInfer :: MonadState InferState (Infer l)

derive newtype instance monadThrowInfer :: MonadThrow (TypeError l) (Infer l)
