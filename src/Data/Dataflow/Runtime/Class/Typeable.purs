module Lunarbox.Data.Dataflow.Runtime.Class.Typeable where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple)
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeArray, typeBool, typeFunction, typeNumber, typePair, typeString)
import Type.Proxy (Proxy(..))

class Typeable (a :: Type) where
  typeof :: Proxy a -> Type

instance typeableNumber :: Typeable Number where
  typeof _ = typeNumber

instance typeableInt :: Typeable Int where
  typeof _ = typeNumber

instance typeableString :: Typeable String where
  typeof _ = typeString

instance typeableBool :: Typeable Boolean where
  typeof _ = typeBool

instance typeableArray :: Typeable a => Typeable (Array a) where
  typeof _ = typeArray (typeof (Proxy :: Proxy a))

instance typePair :: (Typeable a, Typeable b) => Typeable (Tuple a b) where
  typeof _ = typePair (typeof _a) (typeof _b)
    where
    _a :: Proxy a
    _a = Proxy
    
    _b :: Proxy b
    _b = Proxy

instance typeableSymbol :: IsSymbol sym => Typeable (SProxy sym) where
  typeof _ = TVariable true $ TVarName $ reflectSymbol (SProxy :: SProxy sym)

instance typeableArrow :: (Typeable a, Typeable b) => Typeable (a -> b) where
  typeof _ = typeFunction (typeof (Proxy :: Proxy a)) (typeof (Proxy :: Proxy b))

-- | Get the type of a purescript value
getType :: forall a. Typeable a => a -> Type
getType _ = typeof (Proxy :: Proxy a)
