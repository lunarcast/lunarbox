module Lunarbox.Control.Monad.Dataflow.Interpreter
  ( Interpreter(..)
  , InterpreterContext(..)
  , _location
  , runInterpreter
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk, class MonadReader, Reader, runReader)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT, runWriterT)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Dataflow.Runtime.TermEnvironment (TermEnvironment)
import Lunarbox.Data.Dataflow.Runtime.ValueMap (ValueMap)
import Lunarbox.Data.Lens (newtypeIso)

-- The Interpreter context is the env all interpreting occurs in.
newtype InterpreterContext l
  = InterpreterContext
  { location :: l
  , typeEnv :: TermEnvironment
  }

derive instance newtypeInterpreterContent :: Newtype (InterpreterContext l) _

_location :: forall l. Lens' (InterpreterContext l) l
_location = newtypeIso <<< prop (SProxy :: _ "location")

-- Monad used to Interpret expressions
newtype Interpreter l a
  = Interpreter (WriterT (ValueMap l) (Reader (InterpreterContext l)) a)

-- Takes a Interpreter monad and runs it 
runInterpreter :: forall l a. Ord l => Interpreter l a -> ValueMap l
runInterpreter (Interpreter m) = runReader mempty $ runWriterT m

-- Typeclasses
derive instance newtypeInterpreter :: Newtype (Interpreter l a) _

derive newtype instance functorInterpreter :: Ord l => Functor (Interpreter l)

derive newtype instance applyInterpreter :: Ord l => Apply (Interpreter l)

derive newtype instance applicativeInterpreter :: Ord l => Applicative (Interpreter l)

derive newtype instance bindInterpreter :: Ord l => Bind (Interpreter l)

derive newtype instance monadInterpreter :: Ord l => Monad (Interpreter l)

derive newtype instance monadAskInterpreter :: Ord l => MonadAsk (InterpreterContext l) (Interpreter l)

derive newtype instance monadReaderInterpreter :: Ord l => MonadReader (InterpreterContext l) (Interpreter l)

derive newtype instance monadTellInterpreter :: Ord l => MonadTell (ValueMap l) (Interpreter l)

derive newtype instance monadWriterInterpreter :: Ord l => MonadWriter (ValueMap l) (Interpreter l)
