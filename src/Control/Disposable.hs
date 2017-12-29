module Control.Disposable
    ( Disposable -- constructor not exported
    , runDisposable
    , Dispose(..)
    ) where

import Data.IORef
import Data.Semigroup
import qualified GHCJS.Foreign.Callback as J

-- | A wrapper around authorized IO actions.
newtype Disposable = Disposable { runDisposable :: Maybe (IO ()) }

instance Semigroup Disposable where
    (Disposable Nothing) <> f = f
    f <> (Disposable Nothing) = f
    (Disposable (Just f)) <> (Disposable (Just g)) = Disposable (Just (f >> g))

instance Monoid Disposable where
    mempty = Disposable Nothing
    mappend = (<>)

-- | A 'Dispose' is something with some resources to release
class Dispose a where
    dispose :: a -> Disposable

instance Dispose Disposable where
    dispose = id

instance Dispose (J.Callback a) where
    dispose = Disposable . Just . J.releaseCallback
