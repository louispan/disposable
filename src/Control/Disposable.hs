{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Control.Disposable where

import qualified Data.DList as D
import Data.Foldable
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J

-- | A 'Disposable' is something with some resources to release
class Disposable a where
    dispose :: a -> IO ()

instance Disposable (J.Callback a) where
    dispose = J.releaseCallback

-- | Allows storing 'Disposable's in a heterogenous container
data SomeDisposable where
    DisposeNone :: SomeDisposable
    Dispose :: forall a. Disposable a => a -> SomeDisposable
    DisposeList :: forall a. Disposable a => [a] -> SomeDisposable

instance Disposable SomeDisposable where
    dispose DisposeNone = pure ()
    dispose (Dispose a) = dispose a
    dispose (DisposeList as) = traverse_ dispose as

-- | Allow generic deriving instances of things that can be made into 'SomeDisposable'
-- If a data type derives from Generic, and only contain instances of Disposing,
-- then it can also be made an instance of 'Disposing'.
-- Eg.
-- @
-- import Glazier.React as R
-- import GHCJS.Foreign.Callback as J
-- import GHC.Generics as G
--
-- data Plan = Plan
--     { _component :: R.ReactComponent
--     , _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
--     ...
--     } deriving G.Generic
-- instance Disposing Plan
-- @
class Disposing a where
  disposing :: a -> SomeDisposable
  default disposing :: (G.Generic a, GDisposing (G.Rep a)) => a -> SomeDisposable
  disposing x = DisposeList . D.toList . gDisposing $ G.from x

instance Disposing SomeDisposable where
    disposing = id

instance Disposing (J.Callback a) where
    disposing = Dispose

instance Disposing Int where
    disposing _ = DisposeNone

instance Disposing J.JSString where
    disposing _ = DisposeNone

instance Disposing J.JSVal where
    disposing _ = DisposeNone

instance Disposing a => Disposing (D.DList a) where
    disposing xs = DisposeList $ disposing <$> D.toList xs

-- | Generic instance basically traverses the data type structure
-- and expects the values to be all instances of 'Disposing'
class GDisposing f where
    gDisposing :: f p -> D.DList SomeDisposable

instance GDisposing G.U1 where
  gDisposing G.U1 = mempty

instance (GDisposing f, GDisposing g) => GDisposing (f G.:+: g) where
  gDisposing (G.L1 x) = gDisposing x
  gDisposing (G.R1 x) = gDisposing x

instance (GDisposing f, GDisposing g) => GDisposing (f G.:*: g) where
  gDisposing (x G.:*: y) = (gDisposing x) <> (gDisposing y)

instance (Disposing c) => GDisposing (G.K1 i c) where
  gDisposing (G.K1 x) = D.singleton $ disposing x

instance (GDisposing f) => GDisposing (G.M1 i t f) where
  gDisposing (G.M1 x) = gDisposing x
