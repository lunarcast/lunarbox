module Lunarbox.Control.Monad.Dataflow.Interpreter
  ( Interpreter(..)
  , InterpreterContext(..)
  , runInterpreter
  , _location
  , _termEnv
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk, class MonadReader, Reader, runReader)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT, runWriterT)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Dataflow.Runtime (TermEnvironment)
import Lunarbox.Data.Dataflow.Runtime.ValueMap (ValueMap)
import Lunarbox.Data.Lens (newtypeIso)

-- The Interpreter context is the env all interpreting occurs in.
newtype InterpreterContext v l
  = InterpreterContext
  { location :: l
  , termEnv :: TermEnvironment v
  }

derive instance newtypeInterpreterContent :: Newtype (InterpreterContext v l) _

-- Lenses
_location :: forall v l. Lens' (InterpreterContext v l) l
_location = newtypeIso <<< prop (SProxy :: _ "location")

_termEnv :: forall v l. Lens' (InterpreterContext v l) (TermEnvironment v)
_termEnv = newtypeIso <<< prop (SProxy :: _ "termEnv")

-- Monad used to Interpret expressions
newtype Interpreter v l a
  = Interpreter (WriterT (ValueMap v l) (Reader (InterpreterContext v l)) a)

-- Takes a Interpreter monad and runs it 
runInterpreter :: forall v l a. Ord l => Interpreter v l a -> ValueMap v l
runInterpreter (Interpreter m) = runReader mempty $ runWriterT m

-- Typeclasses
derive instance newtypeInterpreter :: Newtype (Interpreter v l a) _

derive newtype instance functorInterpreter :: Ord l => Functor (Interpreter v l)

derive newtype instance applyInterpreter :: Ord l => Apply (Interpreter v l)

derive newtype instance applicativeInterpreter :: Ord l => Applicative (Interpreter v l)

derive newtype instance bindInterpreter :: Ord l => Bind (Interpreter v l)

derive newtype instance monadInterpreter :: Ord l => Monad (Interpreter v l)

derive newtype instance monadAskInterpreter :: Ord l => MonadAsk (InterpreterContext v l) (Interpreter v l)

derive newtype instance monadReaderInterpreter :: Ord l => MonadReader (InterpreterContext v l) (Interpreter v l)

derive newtype instance monadTellInterpreter :: Ord l => MonadTell (ValueMap v l) (Interpreter v l)

derive newtype instance monadWriterInterpreter :: Ord l => MonadWriter (ValueMap v l) (Interpreter v l)
